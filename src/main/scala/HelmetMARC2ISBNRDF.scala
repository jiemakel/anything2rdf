import com.bizo.mighty.csv.CSVReader
import java.net.URLEncoder
import scala.io.Source
import scala.xml.pull._
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
import scala.xml.parsing.XhtmlEntities
import org.apache.jena.vocabulary.DCTerms
import scala.collection.mutable.Stack
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._
import java.io.File
import scala.collection.mutable.HashMap
import org.marc4j.MarcReader
import org.marc4j.MarcStreamReader
import java.io.FileInputStream
import scala.collection.JavaConversions._
import org.marc4j.marc.Record
import org.marc4j.marc.DataField
import java.text.Normalizer

object HelmetMARC2ISBNRDF extends Anything2RDF {

  val sns = "http://foo.bar/schema#"
  val ns = "http://foo.bar/"
  
  val isbnP = P("http://schema.org/isbn") 
  
  implicit def marcReaderToIterator(mr: MarcReader): Iterator[Record] = new Iterator[Record] {
    override def hasNext: Boolean = mr.hasNext
    override def next: Record = mr.next
  }
  
  class NormalizableString(val s: String) {
    def normalize = Normalizer.normalize(s, Normalizer.Form.NFC)
  }

  implicit def stringToNormalizableString(s: String) = new NormalizableString(s)

  
  def getField(field: String, subfield: Char)(implicit record: Record): Seq[String] = {
    record.getVariableFields(field).map(field => field.asInstanceOf[DataField].getSubfields(subfield)).flatten.map(subfield => subfield.getData.normalize.trim)
  }
  
  private val p1 = "^[^a-zA-Z0-9åäöÅÄÖ]*(.*?(?: [a-zA-Z0-9åäöÅÄÖ].)?)[^a-zA-Z0-9åäöÅÄÖ]*$".r
  private val numbers = "[0-9]+".r
  private val isbn = """[0-9\-X]+""".r

  def fix(source: String): String = {
    p1.findFirstMatchIn(source).map(_.group(1)).getOrElse(source)
  }

  def process(records: Iterator[Record])(implicit workAndAuthorNameAndLangToAbstractWorkMap : HashMap[(String,String,String),String]) {
    var books = 0
    var matches = 0
    for (record <- records) {
      books += 1
      implicit val ir = record 
      val physPrefLabels = getField("245", 'a').map(fix)
      val creatorLabels = getField("100", 'a').map(fix)
      val isbns = getField("020", 'a').map(value => isbn.findFirstIn(value).getOrElse(value))
      val languages = getField("041", 'a').map(_ match { 
        case "swe" => "sv"
        case "fin" => "fi"
        case any => any
      })
      for (
          workName <- physPrefLabels;
          authorName <- creatorLabels;
          language <- languages;
          abstractWork <- workAndAuthorNameAndLangToAbstractWorkMap.get((workName,authorName,language));
          isbn <- isbns) {
            m.add(R(abstractWork),isbnP,isbn,language)
            matches += 1
      }
    }
    println("Processed "+books+" books, found "+matches+" matches.");
  }
  
  def main(args: Array[String]): Unit = {
    implicit val workAndAuthorNameAndLangToAbstractWorkMap = new HashMap[(String,String,String),String]
    for (res <- (parse(new File("kirjasampo-qres.json")) \ "results" \ "bindings").children) {
      val JString(workName) = res \ "name" \ "value"
      val JString(lang) = (res \ "name" \ "xml:lang").toOpt.getOrElse(JString(""))
      val JString(authorName) = res \ "author" \ "value"
      val JString(abstractWork) = res \ "aw" \ "value"
      workAndAuthorNameAndLangToAbstractWorkMap.get((workName,authorName,lang)).filter(_!=abstractWork).foreach(s => println("Already found: "+workName+","+authorName+","+lang+","+s))
      workAndAuthorNameAndLangToAbstractWorkMap.put((workName,authorName,lang),abstractWork)
    }
    println("Have records for "+workAndAuthorNameAndLangToAbstractWorkMap.size+" books.");
    process(new MarcStreamReader(new FileInputStream("dumppi1.out")))
    process(new MarcStreamReader(new FileInputStream("dumppi2.out")))
    process(new MarcStreamReader(new FileInputStream("dumppi3.out")))
    process(new MarcStreamReader(new FileInputStream("dumppi4.out")))
    RDFDataMgr.write(new FileOutputStream("kirjasampo-isbn.nt"), m, RDFFormat.NT)
  }
}
