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
  val birthDateP = EOP("date of birth")
  val approximateBirthDateP = EOP("approximate date of birth")
  val deathDateP = EOP("date of death")
  val approximateDeathDateP = EOP("approximate date of death")
  val approximateTimeSpan = EOP("approximate time span")
  val qualifiedAssociationP = EOP("qualified assocation")
  
  val GroupType = EC("Group Type")
  val groupTypeP = EOP("group type")

  val RelationshipType = EC("Relationship Type")
  val relationshipTypeP = EOP("relationship type")

  def makeTimeSpan(startQualifier: String, startYear: String, endQualifier: String, endYearP: String): Option[Resource] = {
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
    val endYear = if (endYearP!="NA") endYearP else startYear
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

    wr = CSVReader("SDFB_people.csv")
    wr.next
    //SDFB Person ID,ODNB ID,Display Name,Prefix,First Name,Last Name,Suffix,Title,All Search Names,Gender,Historical Significance,Birth Year Type,Extant Birth Year,Alternate Birth Year,Death Year Type,Extant Death Year,Alternate Death Year,Group List
    //10000023,42,John Abercromby,"",John,Abercromby,"","","John Abercromby, John, John Abercromby",male,Benedictine monk,BF,1561,1561,AF,1561,1561,[]
    //10010033,22932,Valentine Pyne,"",Valentine,Pyne,"","","Valentine Pyne, Valentine, Valentine Pyne",male,naval officer and soldier,IN,1603,1603,IN,1677,1677,[]
    val pidNameMap = new HashMap[Long,String]
    for (w <- wr) {
      pidNameMap.put(w(0).toLong,w(2))
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
      (w(12),w(13)) match {
        case ("11/2/1715",_) => makeTimeSpan(w(11),"1715","November","2",w(11),"1715","November","2").foreach(ts => i.addProperty(birthDateP,ts))
        case ("9/8/1693",_) => makeTimeSpan(w(11),"1693","September","9",w(11),"1693","September","9").foreach(ts => i.addProperty(birthDateP,ts))
        case _ =>
          makeTimeSpan(w(11),w(12),w(11),w(13)).foreach(ts =>
              if (w(11)=="CA") i.addProperty(approximateBirthDateP,ts) 
              else i.addProperty(birthDateP,ts))
      }
      (w(15),w(16)) match {
        case (_,"January 1, 1678") => makeTimeSpan(w(14),w(15),"January","1",w(14),w(15),"January","1").foreach(ts => i.addProperty(deathDateP,ts))
        case ("1710/11",_) => makeTimeSpan(w(14),"1710","November","",w(14),"1710","November","").foreach(ts => i.addProperty(deathDateP,ts))
        case (_,"after 1621") => makeTimeSpan(w(14),"1621",w(14),"1621").foreach(ts => i.addProperty(deathDateP,ts))
        case ("1738/10/12",_) => makeTimeSpan(w(14),"1738","October","12",w(14),"1738","October","12").foreach(ts => i.addProperty(deathDateP,ts))
        case ("1785/11/2",_) => makeTimeSpan(w(14),"1785","November","2",w(14),"1785","November","2").foreach(ts => i.addProperty(deathDateP,ts))
        case ("1750/4/22",_) => makeTimeSpan(w(14),"1750","April","22",w(14),"1750","April","22").foreach(ts => i.addProperty(deathDateP,ts))
        case _ =>   
          makeTimeSpan(w(14),w(15),w(14),w(16)).foreach(ts =>
              if (w(14)=="CA") i.addProperty(approximateDeathDateP,ts) 
              else i.addProperty(deathDateP,ts))
      }
    }
    
    wr = CSVReader("SDFB_PersonNotes.csv")
    wr.next
    //SDFB Contribution ID,Person ID,Note,Citation,Created By,Created At
    //1,10050000,"Katherine Cullen was the ward, and later wife, of Henry Oxinden.  As his ward, she caused a minor scandal when she ran away to London. As his wife, her correspondence makes up a large portion of the Oxinden Letters held in the British Library.","",4,2015-04-08 18:07:07 UTC
    for (w<- wr) {
      val g = m.createResource(ns+"person_"+w(1)) 
      g.addProperty(RDFS.comment,w(2),"en")
      if (w(4)!="") {
        val q = I(ns+"person_note_association_"+w(0),Map("en"->w(2)),RDF.Statement)
        g.addProperty(qualifiedAssociationP,q)
        q.addProperty(RDF.subject,g)
        q.addProperty(RDF.predicate,RDFS.comment)
        q.addProperty(RDF.`object`,w(2),"en")
        q.addProperty(DCTerms.source,w(3))
      }
    }

    wr = CSVReader("SDFB_groups.csv")
    //SDFB Group ID,Name,Description,Start Year Type,Start Year,End Year Type,End Year,Members List (Name with SDFB Person ID)
    //2,Participants in the Field of Cloth of Gold,Participants in the Field of Cloth of Gold,IN,1520,IN,1520,"[""Ralph Egerton (1476)""]"
    //57,Catholics,Catholics,BF,1400,AF,1800,"[""Mary Tudor (1516)"", ""Christopher Perkins (1542)"", ""Henry Tudor (1491)"", ""John Proctor (1521)""]"
    wr.next
    var qac = 0l
    for (w <- wr) {
      val i = I(ns+"group_"+w(0),Map("en"->w(1)),CIDOC.Group)
      ANE(i,DCTerms.description,w(2),"en")
      "\\((\\d*?)\\)".r.findAllMatchIn(w(7)).foreach(g => {
        qac+=1
        val pid = "10000000".substring(0,"10000000".length-g.group(1).length)+g.group(1)
        val p = m.createResource(ns+"person_"+pid)
        val q = I(ns+"person_group_association_"+qac,Map("en"->(pidNameMap(pid.toLong)+" was a member of "+w(1)+" "+makeTitle(w(3),w(4),"","",w(5),w(6),"",""))),RDF.Statement)
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
    
    wr = CSVReader("SDFB_GroupNotes.csv")
    wr.next
    //SDFB Contribution ID,Group ID,Note,Citation,Created By,Created At
    //1,16,"Samuel Johnson wrote, ""The metaphysical poets were men of learning, and, to show their learning was their whole endeavour; but, unluckily resolving to show it in rhyme, instead of writing poetry, they only wrote verses, and, very often, such verses as stood the trial of the finger better than of the ear; for the modulation was so imperfect, that they were only found to be verses by counting the syllables... The most heterogeneous ideas are yoked by violence together; nature and art are ransacked for illustrations, comparisons, and allusions; their learning instructs, and their subtlety surprises; but the reader commonly thinks his improvement dearly bought, and, though he sometimes admires, is seldom pleased.""","Samuel Johnson, Lives of the Most Eminent English Poets, vol. 1 (1779)",10,2015-04-07 19:55:53 UTC
    for (w<- wr) {
      val g = m.createResource(ns+"group_"+w(1)) 
      g.addProperty(RDFS.comment,w(2),"en")
      if (w(4)!="") {
        val q = I(ns+"group_note_association_"+w(0),Map("en"->w(2)),RDF.Statement)
        g.addProperty(qualifiedAssociationP,q)
        q.addProperty(RDF.subject,g)
        q.addProperty(RDF.predicate,RDFS.comment)
        q.addProperty(RDF.`object`,w(2),"en")
        q.addProperty(DCTerms.source,w(3))
      }
    }
    
    wr = CSVReader("SDFB_GroupCategories.csv")
    wr.next
    //SDFB Group Category ID,Group Category Name,Description,Created By,Created At
    //1,"Intellectual\n",,3,2015-02-25 00:51:20 UTC
    for (w<-wr) {
      val gc = I(ns+"groupType_"+w(0),Map("en"->w(1)),GroupType)
      ANE(gc,DCTerms.description,w(2))
    }
    
    wr = CSVReader("SDFB_GroupCategoryAssignments.csv")
    wr.next
    //SDFB Assignment ID,Group Category ID,Group ID,Created By,Created At
    //1,1,1,3,2015-02-25 00:52:11 UTC
    for (w<-wr)
      m.createResource(ns+"group_"+w(2)).addProperty(groupTypeP,m.createResource(ns+"groupType_"+w(1)))
    
    wr = CSVReader("SDFB_RelationshipTypes.csv")
    wr.next
    //SDFB Relationship Type ID,Relationship Type Name,Description,Relationship Type Inverse,Created By,Created At
    val relNameMap: HashMap[Long,String] = new HashMap[Long,String] 
    for (w <- wr) {
      lm.foreach(p => {
        val i = I(sns+"relationship_"+p._1+"_"+w(0),Map("en"->(p._2+w(1))),OWL.ObjectProperty)
        ANE(i,DCTerms.description,(p._2+w(2)),"en")
        i.addProperty(OWL.inverseOf,m.createResource(sns+"relationship_"+p._1+"_"+w(3)))
      })
      relNameMap.put(w(0).toLong,w(1).toLowerCase)
      val i = I(sns+"relationship_"+w(0),Map("en"->w(1).toLowerCase),OWL.ObjectProperty)
      ANE(i,DCTerms.description,w(2),"en")
      i.addProperty(OWL.inverseOf,m.createResource(sns+"relationship_"+w(3)))
    }
    
    wr = CSVReader("SDFB_RelationshipCategories.csv")
    wr.next
    //SDFB Relationship Category ID,Relationship Category Name,Description,Created By,Created At
    //1,"Affective",,3,2015-02-25 00:51:33 UTC
    for (w<-wr) {
      val gc = I(ns+"relationshipType_"+w(0),Map("en"->w(1)),RelationshipType)
      ANE(gc,DCTerms.description,w(2))
    }
    
    wr = CSVReader("SDFB_RelCategoryAssignments.csv")
    wr.next
    //SDFB Assignment ID,Relationship Category ID,Relationship Type ID,Created By,Created At
    //1,1,1,3,2015-02-25 00:52:00 UTC
    for (w<-wr) lm.foreach(p => m.createResource(sns+"relationship_"+p._1+"_"+w(2)).addProperty(relationshipTypeP,m.createResource(ns+"relationshipType_"+w(1))))

    val relMap: HashMap[Long,(Long,Long)] = new HashMap[Long,(Long,Long)] 
    for (file <- new File(".").listFiles().filter(_.getName.startsWith("SDFB_relationships_"))) {
      //SDFB Relationship ID,Person 1 ID,Person 2 ID,Original Confidence,Maximum Confidence,Start Year Type,Start Day,Start Month,Start Year,End Year Type,End Month,End Day,End Year
      //100000004,10000001,10012160,52,52,AF/IN,"",,1509,BF/IN,"",,1560
      //100000038,10000010,10000772,43,43,AF/IN,,,1595,BF/IN,,,1633
      wr = CSVReader(file.getName)
      wr.next
      for (w <- wr)
        relMap.put(w(0).toLong,(w(1).toLong,w(2).toLong))
    }
    
    for (file <- new File(".").listFiles().filter(_.getName.startsWith("SDFB_RelTypeAssignments_"))) {
      wr = CSVReader(file.getName)
      if (wr.hasNext()) {
        wr.next
        //SDFB Relationship Type Assignment ID,SDFB Relationship ID,SDFB Relationship Type ID,Confidence,Start Year Type,Start Month,Start Day,Start Year,End Date Type,End Month,End Day,End Year
        //13,100165527,15,100,IN,May,29,1630,IN,February,13,1662
        for (w <- wr) {
          val people = relMap(w(1).toLong)
          val p1 = m.createResource(ns+"person_"+people._1)
          val p2 = m.createResource(ns+"person_"+people._2)
          val (id, certaintyDescription) = toDescriptiveCertainty(w(3).toInt)
          val p = P(sns+"relationship_"+id+"_"+w(2))
          val q = I(ns+"person_person_association_"+w(0),Map("en"->(pidNameMap(people._1)+" "+certaintyDescription+relNameMap(w(2).toLong)+" "+pidNameMap(people._2)+" "+makeTitle(w(4),w(7),w(5),w(6),w(8),w(11),w(9),w(10)))),RDF.Statement)
          p1.addProperty(qualifiedAssociationP,q)
          q.addProperty(RDF.subject,p1)
          q.addProperty(RDF.predicate,p)
          q.addProperty(RDF.`object`,p2)
          q.addProperty(probabilityP,w(3),XSDDatatype.XSDdecimal)
          makeTimeSpan(w(4),w(7),w(5),w(6),w(8),w(11),w(9),w(10)).foreach(ts =>
            if (w(4)=="CA" || w(8)=="CA") q.addProperty(approximateTimeSpan,ts) 
            else q.addProperty(CIDOC.has_timeSpan,ts))
          p1.addProperty(p,p2)
        }
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
