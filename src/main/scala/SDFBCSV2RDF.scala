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
import com.hp.hpl.jena.vocabulary.DCTerms
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import scala.util.control.Breaks._
import com.hp.hpl.jena.rdf.model.Property
import scala.util.Try
import org.apache.jena.iri.IRIFactory
import java.io.File

object SDFBCSV2RDF extends Anything2RDF {

  val sns = "http://ldf.fi/sdfb/schema#"
  val ns = "http://ldf.fi/sdfb/"
  
  val odnbIdP = EDP("ODBN id")
  val prefixP = EDP("prefix")
  val suffixP = EDP("suffix")
  val propertyP = EOP("property")
  val probabilityP = EDP("probability")
  val approximateTimeSpan = EOP("approximate time span")
  val qualifiedAssociationP = EOP("qualified assocation")
  
  def makeTimeSpan(startQualifier: String, startYear: String, endQualifier: String, endYear: String): Option[Resource] = {
    var bob, eoe, eob, boe: Option[String] = None
    if (startYear!="") startQualifier match {
      case "AF/IN" => bob = Some(makeDateTime(startYear, "", "")._1)
      case "AF" => bob = Some(makeDateTime(startYear, "", "")._2)
      case "BF/IN" => eob = Some(makeDateTime(startYear, "", "")._2)
      case "BF" => eob = Some(makeDateTime(startYear, "", "")._1)
      case "IN" | "CA" => 
        val dt = makeDateTime(startYear, "", "")
        bob = Some(dt._1)
        eob = Some(dt._2)
    }
    if (endYear!="") endQualifier match {
      case "AF/IN" => boe = Some(makeDateTime(endYear, "", "")._1)
      case "AF" => boe = Some(makeDateTime(endYear, "", "")._2)
      case "BF/IN" => eoe = Some(makeDateTime(endYear, "", "")._2)
      case "BF" => eoe = Some(makeDateTime(endYear, "", "")._1)
      case "IN" | "CA" => 
        val dt = makeDateTime(endYear, "", "")
        boe = Some(dt._1)
        eoe = Some(dt._2)
    }
    if (startYear=="" && endYear=="") None
    else Some(makeTimeSpan(makeTitle(startQualifier,startYear,"","",endQualifier,endYear,"",""), bob, eob, boe, eoe))
  }
  
  def mapMonth(monthName: String): String = {
    monthName match {
      case "January" => "01"
      case "February" => "02"
      case "March" => "03"
      case "April" => "04"
      case "May" => "05"
      case "June" => "06"
      case "July" => "07"
      case "August" => "08"
      case "September" => "09"
      case "October" => "10"
      case "November" => "11"
      case "December" => "12"
      case _ => ""
    }
  }
  
  def mapQualifier(qualifier: String): String = {
    qualifier match {
      case "AF/IN" => "after the beginning of "
      case "AF" => "after "
      case "BF/IN" => "before the end of " 
      case "BF" => "before "
      case "IN" | "CA" => ""
    }
  }
  
  def makeTitle(startQualifier: String, startYear: String, startMonth: String, startDate: String, endQualifier: String, endYear: String, endMonth: String, endDate: String): String = {
    val sb = new StringBuilder()
    if (startYear!="") {
      sb.append(mapQualifier(startQualifier))
      if (startMonth!="") {
        sb.append(startMonth)
        sb.append(' ')
        if (startDate!="") {
          sb.append(startDate)
        sb.append(' ')
        }
      }
      sb.append(startYear)
      sb.append(' ')
    }
    sb.append('-')
    if (endYear!="") {
      sb.append(' ')
      sb.append(mapQualifier(endQualifier))
      if (endMonth!="") {
        sb.append(endMonth)
        sb.append(' ')
        if (endDate!="") {
          sb.append(endDate)
        sb.append(' ')
        }
      }
      sb.append(endYear)
    }
    return sb.toString
  }
  
  def makeTimeSpan(startQualifier: String, startYear: String, startMonth: String, startDate: String, endQualifier: String, endYear: String, endMonth: String, endDate: String): Option[Resource] = {
    var bob, eoe, eob, boe: Option[String] = None
    if (startYear!="") startQualifier match {
      case "AF/IN" => bob = Some(makeDateTime(startYear, mapMonth(startMonth), startDate)._1)
      case "AF" => bob = Some(makeDateTime(startYear, mapMonth(startMonth), startDate)._2)
      case "BF/IN" => eob = Some(makeDateTime(startYear, mapMonth(startMonth), startDate)._2)
      case "BF" => eob = Some(makeDateTime(startYear, mapMonth(startMonth), startDate)._1)
      case "IN" | "CA" => 
        val dt = makeDateTime(startYear, mapMonth(startMonth), startDate)
        bob = Some(dt._1)
        eob = Some(dt._2)
    }
    if (endYear!="") endQualifier match {
      case "AF/IN" => boe = Some(makeDateTime(endYear, mapMonth(endMonth), endDate)._1)
      case "AF" => boe = Some(makeDateTime(endYear, mapMonth(endMonth), endDate)._2)
      case "BF/IN" => eoe = Some(makeDateTime(endYear, mapMonth(endMonth), endDate)._2)
      case "BF" => eoe = Some(makeDateTime(endYear, mapMonth(endMonth), endDate)._1)
      case "IN" | "CA" => 
        val dt = makeDateTime(endYear, mapMonth(endMonth), endDate)
        boe = Some(dt._1)
        eoe = Some(dt._2)
    }
    Some(makeTimeSpan(makeTitle(startQualifier,startYear,startMonth,startDate,endQualifier,endYear,endMonth,endDate), bob, eob, boe, eoe))
  }
  
  val lm = Seq((19,"very unlikely "),(39,"unlikely "),(59,"possibly "),(79,"likely "),(99,"very likely "),(100,""))

  def toDescriptiveCertainty(probability: Int): (Int,String) = {
    lm.foreach(p => if (probability<=p._1) return p)
    throw new IllegalArgumentException("Weird probability: "+probability)
  }

  def main(args: Array[String]): Unit = {
    var wr : CSVReader = null
    //var headers: Array[String] = null
    //var h: Map[String,Int] = null
    wr = CSVReader("SDFB_RelationshipTypes.csv")
    //headers = wr.next
    wr.next
    //h = headers.zipWithIndex.toMap
    lm.dropRight(1).foreach(p => {
      val i = I(sns+"relationship_"+p._1+"_knows",Map("en"->(p._2+"knows")),OWL.ObjectProperty)
      i.addProperty(OWL.inverseOf,i)
    })
    for (w <- wr) {
      lm.foreach(p => {
        val i = I(sns+"relationship_"+p._1+"_"+w(0),Map("en"->(p._2+w(1))),OWL.ObjectProperty)
        ANE(i,DCTerms.description,(p._2+w(2)),"en")
        i.addProperty(OWL.inverseOf,m.createResource(sns+"relationship_"+p._1+"_"+w(3)))
      })
      val i = I(sns+"relationship_"+w(0),Map("en"->w(1)),OWL.ObjectProperty)
      ANE(i,DCTerms.description,w(2),"en")
      i.addProperty(OWL.inverseOf,m.createResource(sns+"relationship_"+w(3)))
    }

    //SDFB Person ID,ODNB ID,Display Name,Prefix,First Name,Last Name,Suffix,Title,All Search Names,Gender,Historical Significance,Birth Year Type,Extant Birth Year,Alternate Birth Year,Death Year Type,Extant Death Year,Alternate Death Year,Group List
    //10000023,42,John Abercromby,"",John,Abercromby,"","","John Abercromby, John, John Abercromby",male,Benedictine monk,BF,1561,1561,AF,1561,1561,[]
    //10010033,22932,Valentine Pyne,"",Valentine,Pyne,"","","Valentine Pyne, Valentine, Valentine Pyne",male,naval officer and soldier,IN,1603,1603,IN,1677,1677,[]
    wr = CSVReader("SDFB_people.csv")
    wr.next
    val pidNameMap = new HashMap[String,String]
    for (w <- wr) {
      pidNameMap.put(w(0),w(2))
      val i = I(ns+"person_"+w(0),Map("en"->w(2)),CIDOC.Person)
      ANE(i,odnbIdP, w(1))
      ANE(i,prefixP,w(3))
      ANE(i,FOAF.givenname,w(4))
      ANE(i,FOAF.family_name,w(5))
      ANE(i,suffixP,w(6))
      ANE(i,FOAF.title,w(7))
      w(8).split(',').map(_.trim).filter(!_.isEmpty).foreach(i.addProperty(SKOS.altLabel,_))
      w(9) match {
        case "male" => i.addProperty(FOAF.gender,SDMXCode.sexMale)
        case "female" => i.addProperty(FOAF.gender,SDMXCode.sexFemale)
        case oth => logger.warn("Unknown gender "+oth+" for "+w.mkString(", "))
      }
      ANE(i,DCTerms.description,w(10),"en")
    }

    //SDFB Group ID,Name,Description,Start Year Type,Start Year,End Year Type,End Year,Members List (Name with SDFB Person ID)
    //2,Participants in the Field of Cloth of Gold,Participants in the Field of Cloth of Gold,IN,1520,IN,1520,"[""Ralph Egerton (1476)""]"
    //57,Catholics,Catholics,BF,1400,AF,1800,"[""Mary Tudor (1516)"", ""Christopher Perkins (1542)"", ""Henry Tudor (1491)"", ""John Proctor (1521)""]"
    wr = CSVReader("SDFB_groups.csv")
    wr.next
    var qac = 0l
    for (w <- wr) {
      val i = I(ns+"group_"+w(0),Map("en"->w(1)),CIDOC.Group)
      ANE(i,DCTerms.description,w(2),"en")
      "\\((\\d*?)\\)".r.findAllMatchIn(w(7)).foreach(g => {
        qac+=1
        val pid = "10000000".substring(0,"10000000".length-g.group(1).length)+g.group(1)
        val p = m.createResource(ns+"person_"+pid)
        val q = I(ns+"person_group_association_"+qac,Map("en"->(pidNameMap(pid)+" was a member of "+w(1)+" "+makeTitle(w(3),w(4),"","",w(5),w(6),"",""))),RDF.Statement)
        p.addProperty(qualifiedAssociationP,q)
        q.addProperty(RDF.subject,p)
        q.addProperty(RDF.predicate,CIDOC.is_current_or_former_member_of)
        q.addProperty(RDF.`object`,i)
        makeTimeSpan(w(3),w(4),w(5),w(6)).foreach(ts =>
          if (w(3)=="CA" || w(5)=="CA") q.addProperty(approximateTimeSpan,ts) 
          else q.addProperty(CIDOC.has_timeSpan,ts))
        p.addProperty(CIDOC.is_current_or_former_member_of,i)
      })
    }
    qac = 0l
    for (file <- new File(".").listFiles().filter(_.getName.startsWith("SDFB_relationships_"))) {
      //SDFB Relationship ID,Person 1 ID,Person 2 ID,Original Confidence,Maximum Confidence,Start Year Type,Start Day,Start Month,Start Year,End Year Type,End Month,End Day,End Year
      //100000004,10000001,10012160,52,52,AF/IN,"",,1509,BF/IN,"",,1560
      //100000038,10000010,10000772,43,43,AF/IN,,,1595,BF/IN,,,1633
      wr = CSVReader(file.getName)
      wr.next
      for (w <- wr) {
        val p1 = m.createResource(ns+"person_"+w(1))
        val p2 = m.createResource(ns+"person_"+w(2))
        val (id, certaintyDescription) = toDescriptiveCertainty(w(4).toInt)
        val p = if (id==100) FOAF.knows else P(sns+"relationship_"+id+"_knows")
        qac+=1
        val q = I(ns+"person_person_association_"+qac,Map("en"->(pidNameMap(w(1))+" "+certaintyDescription+"knew "+pidNameMap(w(2))+" "+makeTitle(w(5),w(7),w(6),w(8),w(9),w(12),w(10),w(11)))),RDF.Statement)
        p1.addProperty(qualifiedAssociationP,q)
        q.addProperty(RDF.subject,p1)
        q.addProperty(RDF.predicate,p)
        q.addProperty(RDF.`object`,p2)
        q.addProperty(probabilityP,w(3),XSDDatatype.XSDdecimal)
        makeTimeSpan(w(5),w(7),w(6),w(8),w(9),w(12),w(10),w(11)).foreach(ts =>
          if (w(3)=="CA" || w(5)=="CA") q.addProperty(approximateTimeSpan,ts) 
          else q.addProperty(CIDOC.has_timeSpan,ts))
        p1.addProperty(p,p2)
      }
    }
    m.setNsPrefixes(PrefixMapping.Standard)
    m.setNsPrefix("dcterms",DCTerms.NS)
    m.setNsPrefix("crm",CIDOC.ns)
    m.setNsPrefix("skos",SKOS.ns)
    m.setNsPrefix("foaf",FOAF.NS)
    m.setNsPrefix("sdmxc",SDMXCode.ns)
    m.setNsPrefix("sdfb",ns)
    m.setNsPrefix("sdfbs",sns)
    RDFDataMgr.write(new FileOutputStream("sdfb.ttl"), m, RDFFormat.TTL)
  }
}
