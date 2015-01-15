import com.bizo.mighty.csv.CSVReader
import java.net.URLEncoder
import org.apache.jena.riot.RDFFormat
import org.apache.jena.riot.RDFDataMgr
import java.io.FileOutputStream
import com.hp.hpl.jena.rdf.model.ResourceFactory
import com.hp.hpl.jena.rdf.model.Resource
import com.hp.hpl.jena.rdf.model.ModelFactory
import com.hp.hpl.jena.rdf.model.Model
import com.hp.hpl.jena.vocabulary.RDF
import com.hp.hpl.jena.vocabulary.OWL
import com.hp.hpl.jena.vocabulary.DC
import com.hp.hpl.jena.vocabulary.DC_11
import com.hp.hpl.jena.vocabulary.RDFS
import com.hp.hpl.jena.sparql.vocabulary.FOAF
import com.github.nscala_time.time.Imports._
import org.joda.time.format.ISODateTimeFormat
import com.hp.hpl.jena.shared.PrefixMapping
import com.hp.hpl.jena.datatypes.xsd.XSDDatatype
import scala.util.control.Breaks._
import scala.collection.mutable.HashMap
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._
import java.io.File
import com.hp.hpl.jena.vocabulary.DCTerms

object GRANDTOURJSON2RDF extends CSV2RDF {
  
  val sns = "http://ldf.fi/grandtour-schema#"
  val ns = "http://ldf.fi/grandtour/"

  /*
   * person=name,bio,occupations,marriages,parent,bioDates,tours,narrative,notes=cites
   * marriage=year,temporalrelationship,participants
   * tour=explanation,stays,links?
   * stay=name,city,from,to,order
   * city=name,coordinates
  */
    
  val Occupation = EC("Occupation")
  val occupationP = EOP("occupation")
  val Marriage = EC("Marriage")
  Marriage.addProperty(RDFS.subClassOf, CIDOC.Joining)
  
  val estimatedTimeSpan = EOP("estimated time-span")
  
  val dob = EOP("time of birth")
  val cdob = EOP("approximate time of birth")
  
  val dod = EOP("time of death")
  val cdod = EOP("approximate time of death")
  
  val Travel = EC("Travel")
  Travel.addProperty(RDFS.subClassOf,CIDOC.Move)
  
  def processCity(prefix:String,person:String,city : JValue): Resource = {
    val JInt(index) = city \ "sequence"
    val t = m.createResource(prefix+index)
    t.addProperty(RDF.`type`,Travel)
    val JString(placeName) = city \ "place"
    val p = I(ns+"location_"+encode(placeName),Map("en"->placeName),CIDOC.Place)
    city \ "coordinates" \ "lat" match {
      case JDouble(lat) => p.addProperty(WGS84.lat,""+lat)
      case _ =>
    }
    city \ "coordinates" \ "lng" match {
      case JDouble(long) => p.addProperty(WGS84.long,""+long)
      case _ =>
    }
    t.addProperty(CIDOC.moved_to,p)
    city \ "children" match {
      case JArray(cities) => cities.foreach(c => t.addProperty(CIDOC.contains_period,processCity(prefix+index+"_",person,c)))
      case _ =>
    }
    val from = city \ "from" \ "raw" match {
      case JString(from) => Some(from)
      case _ => None
    }
    val to = city \ "to" \ "raw" match {
      case JString(to) => Some(to)
      case _ => None
    }
    if (from.isDefined || to.isDefined) {
      var tname = if (from.isDefined) {
        if (to.isDefined) from.get+" - "+to.get else from.get 
      } else to.get
      val time = makeTimeSpan(tname, from.map(_+"T00:00:00.000"), from.map(_+"T23:59:59.999"), to.map(_+"T00:00:00.000"), to.map(_+"T23:59:59.999")) 
      t.addProperty(CIDOC.has_timeSpan,time)
    }
    val JString(name) = city \ "raw"
    t.addProperty(SKOS.prefLabel,person+" in "+name,"en")    
    t
  }
  
  def main(args: Array[String]): Unit = {
    var wr = CSVReader("grandtour-abbreviations.csv")
    val abbrMap = wr.map(r => (("\\b\\Q"+r(0)+"\\E(?=\\s|$)").r,r(1))).toMap
    for (person <- parse(new File("grandtour.json")).children) {
      val JString(rawName) = person \ "name"
      val name = abbrMap.foldLeft(rawName) { case (r,(abbr,expanded)) => abbr.replaceAllIn(r, expanded) }.trim
      val p = I(ns+"person_"+encode(name),name,CIDOC.Person)
      val bios = for (JArray(a) <- person \ "bio";JString(i) <- a) yield i
      for (rawBio <- bios) {
          val bio = abbrMap.foldLeft(rawBio) { case (r,(abbr,expanded)) => abbr.replaceAllIn(r, expanded) }.trim
          p.addProperty(DCTerms.description,bio,"en")
      }
      val occupations = for (JArray(a) <- person \ "occupations";JString(i) <- a) yield i
      for (occupation <- occupations) p.addProperty(occupationP,I(ns+"occupation_"+encode(occupation),Map("en"->occupation),Occupation))
      val JString(narrative) = person \ "narrative"
      ANE(p,RDFS.comment,narrative,"en")
      person \ "marriages" match {
        case JArray(marriages) => marriages.foldRight(null.asInstanceOf[Resource]) { (marriage,nextMarriage) =>
          val JString(rawSpouse) = marriage \ "spouse"
          val spouse = abbrMap.foldLeft(rawSpouse) { case (r,(abbr,expanded)) => abbr.replaceAllIn(r, expanded) }.trim
          val JInt(count) = marriage \ "sequence"
          val mr = m.createResource(ns+"marriage_"+encode(name)+"_"+count)
          mr.addProperty(RDF.`type`,Marriage)
          if (nextMarriage!=null) {
            nextMarriage.addProperty(CIDOC.occurs_after,mr)
            mr.addProperty(CIDOC.occurs_before,nextMarriage)
          }
          val year = marriage \ "year" match {
            case JString(year) => {
              val (bdate,edate) = makeDateTime(year,"","")
              mr.addProperty(CIDOC.has_timeSpan,makeTimeSpan(year,bdate,edate))
              Some(year)
            }
            case _ => None
          }
          mr.addProperty(SKOS.prefLabel,"Marriage between "+name+" and "+spouse+(year.map(" in "+_).getOrElse("")),"en")
          p.addProperty(CIDOC.joined,mr)
          I(ns+"person_"+encode(spouse),spouse,CIDOC.Person).addProperty(CIDOC.joined,mr)
          mr
        }
        case _ =>
      }
      person \ "parents" match {
        case JString(rawParent) => {
          val parent = abbrMap.foldLeft(rawParent) { case (r,(abbr,expanded)) => abbr.replaceAllIn(r, expanded) }.trim
          p.addProperty(REL.childOf,I(ns+"person_"+encode(parent),parent,CIDOC.Person))
        }
        case _ =>
      }
      person \ "bioDates" match {
        case dates : JObject => {
          val circa = ((dates \ "circa" toOption).isDefined)
          val bdate = dates \ "born" match { case JInt(bdate) => Some(bdate); case _ => None }
          bdate.foreach(d=>p.addProperty(if (circa) cdob else dob,makeTimeSpan(""+d,"","")))
          val ddate = dates \ "dead" match { case JInt(ddate) => Some(ddate); case _ => None }
          ddate.foreach(d=>p.addProperty(if (circa) cdod else dod,makeTimeSpan(""+d,"","")))
        }
        case _ =>
      }
      val JString(cites) = person \ "notes"
      ANE(p,DCTerms.bibliographicCitation,cites)
      person \ "tours" match {
        case JArray(tours) => tours.foldLeft((1,null.asInstanceOf[Resource])) { case ((count,lastTour),tour) => 
          val JString(tourName) = tour \ "raw"
          val t = I(ns+"tour_"+encode(name)+"_"+count,tourName,Travel)
          val bdate = tour \ "startDate" match {
            case sd : JObject => {
              val JInt(fyear) = sd \ "from"
              val JInt(tyear) = sd \ "to"
              Some((""+fyear,""+tyear))
            }
            case _ => None
          }
          val edate = tour \ "endDate" match {
            case sd : JObject => {
              val JInt(fyear) = sd \ "from"
              val JInt(tyear) = sd \ "to"
              Some((""+fyear,""+tyear))
            }
            case _ => None
          }
          var tsname = if (bdate.isDefined) {
             if (bdate.get._1==bdate.get._2) bdate.get._1 else bdate.get._1+"-"+bdate.get._2 
          } else ""
          if (edate.isDefined) {
            if (edate.get._1==edate.get._2) {
              if (edate.get._1!=bdate.get._1 || edate.get._1!=bdate.get._2) tsname+="-"+edate.get._1
            } else tsname+=" - "+edate.get._1+"-"+edate.get._2
          }
          if (!tsname.isEmpty)
            t.addProperty(CIDOC.has_timeSpan,makeTimeSpan(tsname,bdate.map(p => makeDateTime(p._1,"","")._1),bdate.map(p => makeDateTime(p._2,"","")._2),edate.map(p => makeDateTime(p._1,"","")._1),edate.map(p => makeDateTime(p._2,"","")._2)))
          p.addProperty(CIDOC.participated_in,t)
          tour \ "travels" match {
            case JArray(cities) => cities.foreach(c=> t.addProperty(CIDOC.contains_period,processCity(ns+"tour_"+encode(name)+"_"+count+"_",name,c)))
            case _ =>
          }
          (count+1,t)
        }
        case _ => 
      }
    }
    m.setNsPrefixes(PrefixMapping.Standard)
    m.setNsPrefix("crm",CIDOC.ns)
    m.setNsPrefix("skos",SKOS.ns)
    m.setNsPrefix("foaf",FOAF.NS)
    m.setNsPrefix("rel",REL.ns)
    m.setNsPrefix("gt",ns)
    m.setNsPrefix("gts",sns)
    m.setNsPrefix("ee",EECSV2RDF.ns)
    m.setNsPrefix("ees",EECSV2RDF.sns)
    m.setNsPrefix("procope",PROCOPECSV2RDF.ns)
    m.setNsPrefix("procopes",PROCOPECSV2RDF.sns)
    RDFDataMgr.write(new FileOutputStream("grandtour.ttl"), m, RDFFormat.TTL)
  }
}
