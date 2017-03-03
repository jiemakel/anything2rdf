import com.bizo.mighty.csv.CSVReader
import java.net.URLEncoder
import scala.io.Source
import scala.xml.pull._
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
import com.bizo.mighty.csv.CSVDictReader
import scala.xml.parsing.XhtmlEntities
import com.hp.hpl.jena.vocabulary.DCTerms
import java.io.FileInputStream
import com.hp.hpl.jena.datatypes.xsd.XSDDatatype
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._
import scala.collection.mutable.HashSet
import com.hp.hpl.jena.rdf.model.Statement
import scala.collection.mutable.ArrayBuffer
import com.hp.hpl.jena.sparql.vocabulary.FOAF
import java.util.zip.GZIPInputStream
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.concurrent.Await
import scala.concurrent.Future
import java.util.zip.GZIPOutputStream
import com.hp.hpl.jena.rdf.model.Property
import org.apache.jena.riot.system.StreamRDFWriter
import com.hp.hpl.jena.graph.Triple
import com.hp.hpl.jena.graph.NodeFactory
import com.hp.hpl.jena.graph.Node
import com.hp.hpl.jena.datatypes.xsd.XSDDatatype
import com.hp.hpl.jena.vocabulary.XSD

object VIAFXML2RDF extends Anything2RDF {

  val sns = "http://viaf.org/viaf/terms#"
  val ns = "http://viaf.org/viaf/"
  val birthDateP = EOP("date of birth")
  val deathDateP = EOP("date of death")
  val flourishedP = EOP("flourished")
  val relatedLabel = EDP("related label")
  val nationalityP = EOP("nationality")
  val Nationality = EC("Nationality")
  val relatorP = EOP("role")
  val Role = EC("Role")
  val frequencyP = EDP("frequency")

  val s = StreamRDFWriter.getWriterStream(new GZIPOutputStream(new FileOutputStream("viaf.nt.gz")),RDFFormat.NTRIPLES)

  /*4939498 "Corporate"
 515435 "Geographic"
21982545 "Personal"
1550133 "UniformTitleExpression"
2407292 "UniformTitleWork"
   */
  val nameTypeMap = Map("Corporate"->CIDOC.Group,"Personal"->CIDOC.Person,"Geographic"->CIDOC.Place)

  /*  674657 "a"
1802341 "b"
      4 "c"
   3954 "f"
  11238 "m"
28902709 "u"  */
  val genderMap = Map("a"->SDMXCode.sexFemale,"b"->SDMXCode.sexMale,"f"->SDMXCode.sexFemale,"c"->SDMXCode.sexNotApplicable)

  def readContents(implicit xml: XMLEventReader): String = {
    var break = false
    val content = new StringBuilder()
    while (xml.hasNext && !break) xml.next match {
      case EvText(text) => content.append(text)
      case er: EvEntityRef =>
        content.append('&')
        content.append(er.entity)
        content.append(';')
      case EvComment(_) =>
      case EvElemEnd(_,_) => break = true
    }
    return content.toString
  }

  def readAggregate(endTag: String, values: HashMap[String,String])(implicit xml: XMLEventReader): Unit = {
    var break = false
    while (xml.hasNext && !break) xml.next match {
      case EvElemStart(_,"data",attrs,_) =>
        xml.next
        val value = readContents
        values.put(value,if (attrs("count") != null) attrs("count")(0).text else "1")
      case EvElemEnd(_,endTag) => break = true
      case _ =>
    }
  }

  def readAlternate(endTag: String)(implicit xml: XMLEventReader): Option[String] = {
    var break = false
    val content = new StringBuilder()
    while (xml.hasNext && !break) xml.next match {
      case EvElemStart(_,"subfield",attrs,_) if (attrs("code") != null) => attrs("code")(0).text match {
        case "e" | "9" =>
        case _ =>
          content.append(readContents)
          content.append(" ")
      }
      case EvElemStart(_,"subfield",_,_) =>
        content.append(readContents)
        content.append(" ")
      case EvElemEnd(_,endTag) => break = true
      case _ =>
    }
    if (content.length != 0) {
      content.setLength(content.length - 1)
      return Some(content.toString)
    }
    return None
  }

  def partitionDate(date: String): (String, String, String) = {
    val parts = if (date.charAt(0)=='-') {
          val p = date.substring(1).split('-')
          p(0) = "-"+p(0)
          p
        } else
          date.split('-')
     (parts(0), if (parts.length>1) parts(1) else "", if (parts.length>2) parts(2) else "")
  }

  def processDate(r: Node, dateP: Property, dateS: String, circa: Boolean): Unit = {
    if (dateS!="0") {
      val (year, month, date) = partitionDate(dateS)
      val (bob,_) = makeDateTime(if (circa) "" + (year.toInt - 5) else year, month, date)
      val (_,eoe) = makeDateTime(if (circa) "" + (year.toInt + 5) else year, month, date)
        s.triple(new Triple(r,dateP.asNode, makeTimeSpan(dateS, bob, eoe).asNode))
    }
  }

  def process(record: String): Future[Unit] = Future {
    implicit val xml = new XMLEventReader(Source.fromString(record.substring(record.indexOf("\t")+1)))
    var id: String = null
    var nameType: String = ""
    val prefLabels: HashSet[String] = new HashSet[String]()
    val altLabels: HashSet[String] = new HashSet[String]()
    //val relLabels: HashSet[String] = new HashSet[String]()
    var birthDate: String = ""
    var deathDate: String = ""
    var dateType: String = ""
    var gender: String = ""
    val nationalities: HashMap[String,String] = new HashMap[String,String]()
    //val countries: HashMap[String,String] = new HashMap[String,String]()
    val relatorCodes: HashMap[String,String] = new HashMap[String,String]()
    while (xml.hasNext) xml.next match {
      case EvElemStart(_,"viafID",_,_) => id = readContents
      case EvElemStart(_,"nameType",_,_) => nameType = readContents
      case EvElemStart(_,"mainHeadings",_,_) =>
        var break = false
        while (xml.hasNext && !break) xml.next match {
          case EvElemStart(_,"text",_,_) => prefLabels.add(readContents)
          case EvElemEnd(_,"mainHeadings") => break = true
          case _ =>
        }
      case EvElemStart(_,"dateType",_,_) => dateType = readContents
      case EvElemStart(_,"gender",_,_) => gender = readContents
      case EvElemStart(_,"birthDate",_,_) => birthDate = readContents
      case EvElemStart(_,"deathDate",_,_) => deathDate = readContents
      case EvElemStart(_,"x400",_,_) => readAlternate("x400").foreach(altLabels.add(_))
      //case EvElemStart(_,"x500",_,_) => readAlternate("x500").foreach(relLabels.add(_))
      case EvElemStart(_,"nationalityOfEntity",_,_) => readAggregate("nationalityOfEntity", nationalities)
      //case EvElemStart(_,"countries",_,_) => readAggregate("countries", countries)
      case EvElemStart(_,"RelatorCodes",_,_) => readAggregate("RelatorCodes", relatorCodes)
      case _ =>
    }
    nameTypeMap.get(nameType).foreach(t => m.synchronized {
      val r = RN(ns+id)
      s.triple(new Triple(r,RDF.`type`.asNode,t.asNode))
      for (prefLabel <- prefLabels) s.triple(new Triple(r,SKOS.prefLabel.asNode, LN(prefLabel)))
      for (altLabel <- altLabels; if !prefLabels.contains(altLabel)) s.triple(new Triple(r,SKOS.altLabel.asNode, LN(altLabel)))
      //for (relLabel <- relLabels if !prefLabels.contains(relLabel) && !altLabels.contains(relLabel)) s.triple(new Triple(r,relatedLabel.asNode, LN(relLabel)))
      genderMap.get(gender).foreach(g => s.triple(new Triple(r,FOAF.gender.asNode, g.asNode)))
      for ((nationality,frequency) <- nationalities; if nationality!="XX") {
        val n = I(ns+"nationality_"+encode(nationality),nationality,Nationality)
        s.triple(new Triple(r,nationalityP.asNode, n.asNode))
/*        if (frequency!="1") {
          val st = BN()
          s.triple(new Triple(st,RDF.`type`.asNode, RDF.Statement.asNode))
          s.triple(new Triple(st,RDF.subject.asNode, r))
          s.triple(new Triple(st,RDF.predicate.asNode, nationalityP.asNode))
          s.triple(new Triple(st,RDF.`object`.asNode, n.asNode))
          s.triple(new Triple(st,frequencyP.asNode, NodeFactory.createLiteral(frequency,XSDDatatype.XSDinteger)))
        }*/
      }
      for ((relatorCode,frequency) <- relatorCodes) {
        val n = I(ns+"role_"+encode(relatorCode),relatorCode,Role)
        s.triple(new Triple(r,relatorP.asNode, n.asNode))
/*        if (frequency!="1") {
          val st = BN()
          s.triple(new Triple(st,RDF.`type`.asNode, RDF.Statement.asNode))
          s.triple(new Triple(st,RDF.subject.asNode, r))
          s.triple(new Triple(st,RDF.predicate.asNode, relatorP.asNode))
          s.triple(new Triple(st,RDF.`object`.asNode, n.asNode))
          s.triple(new Triple(st,frequencyP.asNode, NodeFactory.createLiteral(frequency,XSDDatatype.XSDinteger)))
        }*/
      }
  /*
  106024 "circa"
 873360 "flourished"
30415519 "lived"*/

      dateType match {
        case "flourished" =>
          if (birthDate!="0") {
            val (byear, bmonth, bdate) = partitionDate(birthDate)
            val (eyear, emonth, edate) = if (deathDate!="0") partitionDate(deathDate) else (byear, bmonth, bdate)
            val name = if (deathDate!="0" && deathDate!=birthDate) birthDate+"-"+deathDate else birthDate
            s.triple(new Triple(r,flourishedP.asNode,makeTimeSpan(name, makeDateTime(byear,bmonth,bdate),makeDateTime(eyear,emonth,edate)).asNode))
          }
        case "lived" =>
          processDate(r, birthDateP, birthDate, false)
          processDate(r, deathDateP, deathDate, false)
        case "circa" =>
          processDate(r, birthDateP, birthDate, true)
          processDate(r, deathDateP, deathDate, true)
      }
    })
//      output.write(Seq(id,nameType,birthDate,deathDate,gender,countries.map(p => p._1+":"+p._2).mkString(";"),nationalities.map(p => p._1+":"+p._2).mkString(";"),relatorCodes.map(p => p._1+":"+p._2).mkString(";"),prefLabels.map(_.replace(";","\\;")).mkString(";"),altLabels.map(_.replace(";","\\;")).mkString(";"),relLabels.map(_.replace(";","\\;")).mkString(";")))
  }

  val numWorkers = sys.runtime.availableProcessors
  val queueCapacity = 2

  implicit val ec = ExecutionContext.fromExecutorService(
   new ThreadPoolExecutor(
     numWorkers, numWorkers,
     0L, TimeUnit.SECONDS,
     new ArrayBlockingQueue[Runnable](queueCapacity) {
       override def offer(e: Runnable) = {
         put(e); // may block if waiting for empty room
         true
       }
     }
   )
  )

  def main(args: Array[String]): Unit = {
    val st = Source.fromInputStream(new GZIPInputStream(new FileInputStream("viaf.xml.gz")), "UTF-8")
    s.start()
    val f = Future.sequence(for (record <- st.getLines) yield process(record))
    f.onFailure { case t => logger.error("Processing of at least one linr resulted in an error:" + t.getMessage+": " + t.printStackTrace) }
    f.onSuccess { case _ => logger.info("Successfully processed all lines.") }
    Await.result(f, Duration.Inf)
    s.finish()
    m.setNsPrefix("crm", CIDOC.ns)
    m.setNsPrefix("viaf", ns)
    m.setNsPrefix("viaf-schema", sns)
    m.setNsPrefix("skos", SKOS.ns)
    m.setNsPrefix("xsd", XSD.getURI())
    RDFDataMgr.write(new FileOutputStream("viaf-ontology.ttl"), m, RDFFormat.TTL)
  }
}
