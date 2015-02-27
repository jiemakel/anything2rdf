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
import com.bizo.mighty.csv.CSVDictReader
object GRANDTOURCSV2RDF extends Anything2RDF {
  
  val sns = "http://ldf.fi/grandtour-schema#"
  val ns = "http://ldf.fi/grandtour/"
  
  val Expertise = EC("Expertise")
  val expertiseP = EOP("expertise")
  
  val Education = EC("Education")
  val educationP = EOP("education")

  val maidenName = EDP("maiden name")
  
  val passingThrough = EDP("passing through")
  
  val estimatedTimeSpan = EOP("estimated time-span")
  
  val Travel = EC("Travel")
  
  Travel.addProperty(RDFS.subClassOf,CIDOC.Event)
  
  def main(args: Array[String]): Unit = {
    var wr1 = CSVReader("grandtour-abbreviations.csv")
    val abbrMap = wr1.map(r => (r(0),r(1))).toMap
    wr1 = CSVReader("grandtour-expertises.csv")
    for (w <- wr1) I(s"${ns}expertise_${w(0)}",Map("en"->w(1)),Expertise)
    var wr = CSVDictReader("grandtour-people.csv")
    val idNameMap = new HashMap[String,String]
    for (r <- wr) {
      val name = s"${r("Name_First")}${if (!r("Name_First").isEmpty() && !r("Name_Last").isEmpty()) " " else ""}${r("Name_Last")}"
      idNameMap+=((r("AccessID"),name))
      val person = I(ns+"person_"+r("AccessID"),name,CIDOC.Person)
      ANE(person,FOAF.firstName,r("Name_First"))
      ANE(person,FOAF.family_name,r("Name_Last"))
      ANE(person,maidenName,r("Name_Maiden"))
      ANE(person,REL.spouseOf,r("Name of Spouse"))
      if (!r("Gender").trim.isEmpty()) r("Gender") match {
        case "M" | "m" => person.addProperty(FOAF.gender,SDMXCode.sexMale)
        case "F" | "f" => person.addProperty(FOAF.gender,SDMXCode.sexFemale)
        case _ => throw new IllegalArgumentException("Unknown gender "+r("Gender"))
      }
      if (!r("DOB").trim.isEmpty()) {
        if (r("DOB").startsWith("c. ")) {
          val (bdateTime,edateTime) = makeDateTime(r("DOB").substring(3),"","")
          val date = I(s"${PROCOPECSV2RDF.ns}date_${bdateTime}TO${edateTime}",r("DOB").substring(3),CIDOC.TimeSpan)
          person.addProperty(PROCOPECSV2RDF.possibleBDateP,date)
        } else {
          val (bdateTime,edateTime) = makeDateTime(r("DOB"),"","")
          val date = I(s"${PROCOPECSV2RDF.ns}date_${bdateTime}TO${edateTime}",r("DOB"),CIDOC.TimeSpan)
          person.addProperty(PROCOPECSV2RDF.bdateP,date)          
        }
      }
      if (!r("DOD").trim.isEmpty()) {
        if (r("DOD").startsWith("c. ")) {
          val (bdateTime,edateTime) = makeDateTime(r("DOD").substring(3),"","")
          val date = I(s"${PROCOPECSV2RDF.ns}date_${bdateTime}TO${edateTime}",r("DOD").substring(3),CIDOC.TimeSpan)
          person.addProperty(PROCOPECSV2RDF.possibleDDateP,date)
        } else {
          val (bdateTime,edateTime) = makeDateTime(r("DOD"),"","")
          val date = I(s"${PROCOPECSV2RDF.ns}date_${bdateTime}TO${edateTime}",r("DOD"),CIDOC.TimeSpan)
          person.addProperty(PROCOPECSV2RDF.ddateP,date)          
        }
      }
      if (!r("Nationality").trim.isEmpty()) {
        val nationality = I(s"${PROCOPECSV2RDF.ns}nationality_${encode(r("Nationality"))}",Map("en"->r("Nationality")),PROCOPECSV2RDF.Nationality)
        person.addProperty(PROCOPECSV2RDF.nationalityP,nationality)
      }
      if (!r("Title").trim.isEmpty) {
        val titleString = abbrMap.foldLeft(r("Title")) {
          case (r,(abbr,expanded)) => r.replaceAllLiterally(abbr, expanded)
        }
        val title = I(s"${PROCOPECSV2RDF.ns}title_${titleString}",Map("en"->titleString),PROCOPECSV2RDF.Title)
        person.addProperty(PROCOPECSV2RDF.titleP,title)
      }
      if (!r("Expertise1").trim.isEmpty) person.addProperty(expertiseP,ResourceFactory.createResource(s"${ns}expertise_${r("Expertise1")}"))
      if (!r("Expertise2").trim.isEmpty) person.addProperty(expertiseP,ResourceFactory.createResource(s"${ns}expertise_${r("Expertise2")}"))
      if (!r("Expertise3").trim.isEmpty) person.addProperty(expertiseP,ResourceFactory.createResource(s"${ns}expertise_${r("Expertise3")}"))
      ANE(person,RDFS.comment,r("Notes"),"en")
      ANE(person,P(ns+"walpole"),r("Walpole"))
      ANE(person,P(ns+"socialStatus"),r("Social Status"))
      r("Education").trim.split(',').map(s => s.trim).filter(s => !s.isEmpty).foreach(n => {
        val educationString = abbrMap.foldLeft(n) {
          case (r,(abbr,expanded)) => r.replaceAllLiterally(abbr, expanded)
        }
        val education = I(ns+"education_"+encode(educationString),Map("en"->educationString),Education)
        person.addProperty(educationP,education)
      })
      r("Clubs and Academies").trim.split(',').map(s => s.trim).filter(s => !s.isEmpty).foreach(n => {
        val orgString = abbrMap.foldLeft(n) {
          case (r,(abbr,expanded)) => r.replaceAllLiterally(abbr, expanded)
        }
        val organization = I(ns+"organization_"+encode(orgString),Map("en"->orgString),ORG.Organization)
        person.addProperty(CIDOC.is_current_or_former_member_of,organization)
      })
      ANE(person,P(ns+"journal"),r("Journal"))
    }
    wr = CSVDictReader("grandtour-travel.csv")
    var i = 0
    for (r <- wr;
      if (idNameMap.contains(r("NEWID")))
    ) {
      i+=1
      val travel = I(ns+"travel_"+i,Map("en"->s"${idNameMap(r("NEWID"))} in ${r("City_ID")} ${r("Arrival_Date")}-${r("Departure_Date")}"),Travel)
      travel.addProperty(CIDOC.carried_out_by,ResourceFactory.createResource(ns+"person_"+r("NEWID")))
      val date = I(s"${PROCOPECSV2RDF.ns}date_${r("Arrival_Date")}00:00:00.000TO${r("Arrival_Date")}23:59:59.999TO${r("Departure_Date")}00:00:00.000TO${r("Departure_Date")}23:59:59.999",r("Arrival_Date")+" to "+r("Departure_Date"),CIDOC.TimeSpan)
      date.addProperty(CIDOC.begin_of_the_begin,r("Arrival_Date")+"T00:00:00.000",XSDDatatype.XSDdateTime)
      date.addProperty(CIDOC.end_of_the_begin,r("Arrival_Date")+"T23:59:59.999",XSDDatatype.XSDdateTime)
      date.addProperty(CIDOC.begin_of_the_end,r("Departure_Date")+"T00:00:00.000",XSDDatatype.XSDdateTime)
      date.addProperty(CIDOC.end_of_the_end,r("Departure_Date")+"T23:59:59.999",XSDDatatype.XSDdateTime)
      val place = I(s"${PROCOPECSV2RDF.ns}location_${encode(r("City_ID"))}",r("City_ID"),CIDOC.Place)
      travel.addProperty(CIDOC.took_place_at,place)
      if (r("Estimated?")=="1") 
        travel.addProperty(estimatedTimeSpan,date)
        else 
        travel.addProperty(CIDOC.has_timeSpan,date)
      if (r("Just Passing Through?")=="1") travel.addProperty(passingThrough,"true",XSDDatatype.XSDboolean)
       travel.addProperty(passingThrough,"true",XSDDatatype.XSDboolean)
    }
    m.setNsPrefixes(PrefixMapping.Standard)
    m.setNsPrefix("crm",CIDOC.ns)
    m.setNsPrefix("skos",SKOS.ns)
    m.setNsPrefix("foaf",FOAF.NS)
    m.setNsPrefix("gt",ns)
    m.setNsPrefix("gts",sns)
    m.setNsPrefix("da",ns)
    m.setNsPrefix("das",sns)
    m.setNsPrefix("ee",EECSV2RDF.ns)
    m.setNsPrefix("ees",EECSV2RDF.sns)
    m.setNsPrefix("procope",PROCOPECSV2RDF.ns)
    m.setNsPrefix("procopes",PROCOPECSV2RDF.sns)
    
    RDFDataMgr.write(new FileOutputStream("grandtour.ttl"), m, RDFFormat.TTL)
  }
}
