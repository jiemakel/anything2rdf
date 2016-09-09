import com.bizo.mighty.csv.CSVReader
import java.net.URLEncoder
import org.apache.jena.riot.RDFFormat
import org.apache.jena.riot.RDFDataMgr
import java.io.FileOutputStream
import org.apache.jena.rdf.model.ResourceFactory
import org.apache.jena.rdf.model.Resource
import org.apache.jena.rdf.model.ModelFactory
import org.apache.jena.rdf.model.Model
import org.apache.jena.vocabulary.RDF
import org.apache.jena.vocabulary.OWL
import org.apache.jena.vocabulary.DC
import org.apache.jena.vocabulary.DC_11
import org.apache.jena.vocabulary.RDFS
import com.bizo.mighty.csv.CSVDictReader
import com.bizo.mighty.csv.CSVReaderSettings
import scala.io.Source
import org.apache.jena.sparql.vocabulary.FOAF
import scala.xml.pull.XMLEventReader
import scala.xml.pull.EvElemStart
import scala.xml.pull.EvText
import scala.xml.pull.EvElemEnd
import scala.xml.MetaData
import java.util.HashMap
import java.io.File
import java.io.FileInputStream

object YKLXML2RDF extends Anything2RDF {
  
  val sns = "http://ldf.fi/ykl-schema#"
  val ns = "http://ldf.fi/ykl/"
  
  def get(key: String)(implicit attrs: MetaData): Option[String] = {
    if (attrs(key)!=null && attrs(key)(0).text!="") Some(attrs(key)(0).text.trim)
    else None
  }
  
  def process(file: String, lang: String): Unit = {
    val t = new FileInputStream(new File(file))
    t.read
    t.read
    t.read
    val xml = new XMLEventReader(Source.fromInputStream(t))
    var clazz: Resource = null
    var break = false
    while (xml.hasNext && !break) xml.next match {
      case EvElemStart(_,"number", _, _) =>
        val number = xml.next.asInstanceOf[EvText].text
        clazz = R(ns+"class_"+number)
        if (number.length==4) clazz.addProperty(SKOS.broader,R(ns+"class_"+number.substring(0,2)))
        else if (number.length>1) clazz.addProperty(SKOS.broader,R(ns+"class_"+number.substring(0,number.length()-1)))
        clazz.addProperty(RDF.`type`, SKOS.Concept)
        clazz.addProperty(SKOS.prefLabel, number)
      case EvElemStart(_,"name", _, _) => clazz.addProperty(SKOS.prefLabel, xml.next.asInstanceOf[EvText].text, lang)
      case EvElemStart(_,"description", _, _) => clazz.addProperty(SKOS.definition, xml.next.asInstanceOf[EvText].text, lang)
      case EvElemEnd(_,"classes") =>
        break = true
      case _ =>
    }
    t.close()
  }
      
  def main(args: Array[String]): Unit = {
    process("YKL_20160801_041546_fi-FI.xml","fi")
    process("YKL_20160801_041606_sv-FI.xml","sv")
    RDFDataMgr.write(new FileOutputStream("ykl.nt"), m, RDFFormat.NT)
    
  }
}
