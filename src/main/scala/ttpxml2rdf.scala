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

object TTPXML2RDF extends Anything2RDF {

  val sns = "http://ldf.fi/ttp-schema#"
  val ns = "http://ldf.fi/ttp/"

  private val fields = """(?s)\|(.*?)=([^\|]+)""".r

  def process(form : String, title : String, content : String) : Unit = {
    var ct = content
    ct = ct.replaceAll("""\[\[.*?\|(.*?)\]\]""", """$1""")
    ct = ct.replaceAll("""\[\[Oikeustiede:(.*?)\]\]""","""$1""")
    ct = ct.replaceAll("""\[\[(.*?)\]\]""", """$1""")
    val r = I(ns+encode(title),Map("fi"->title),SKOS.Concept)
    form match {
      case "NAMESPACE" =>
      case "Käsite" =>
        for (mt <- fields.findAllMatchIn(ct)) {
          val tag = mt.group(1).toLowerCase()
          val content = mt.group(2).trim
          tag match {
            case "selite_fi" => r.addProperty(RDFS.comment,content,"fi")
            case "määritelmä_fi" => r.addProperty(DCTerms.description,content,"fi")
            case "logotiedosto" | "logolinkki" | "tarkistettu" =>
            case u => //println(u)
          }
        }            
      case "Lähikäsite" => for (mt <- fields.findAllMatchIn(ct)) {
          val tag = mt.group(1).toLowerCase()
          val content = mt.group(2).trim
          tag match {
            case "käsite" => 
              val n2 = content.replaceFirst("^[Oo]ikeustiede:","")
              r.addProperty(SKOS.related,I(ns++encode(n2),Map("fi"->n2),SKOS.Concept))
            case u => //println(u)
          }
      }
      case "Liittyvä nimitys" => 
          var lang : Option[String] = None
          var aname : Option[String] = None
          for (mt <- fields.findAllMatchIn(ct)) {
            val tag = mt.group(1).toLowerCase()
            val content = mt.group(2).trim
            tag match {
              case "kieli" => lang = content match {
                case "suomi" => Some("fi")
                case "ruotsi" => Some("sv")
                case "englanti" => Some("en")
                case "saksa" => Some("de")
                case "ranska" => Some("fr")
                case "latina" => Some("la")
                case "norja" => Some("no")
              }
              case "nimitys" => aname=Some(content) 
              case u => //println(u)
            }
          }
          aname.foreach(n => r.addProperty(SKOS.altLabel,n,lang.getOrElse("fi")))
      case u => println("Unknown form: "+u+": "+content)      
    }
    
  }
  
  def process2(form : String, content : String) : String = {
    if (form.toLowerCase()=="lakilinkki") content.substring(5).replaceFirst("^0+","")+"/"+content.substring(1,5)
    else if (form=="NAMESPACE") "Oikeustiede"    
    else {
     println("Unknown subtag: "+form)
     content
    } 
  }

  def main(args: Array[String]): Unit = {
    val xml = new XMLEventReader(Source.fromFile("Tieteen+termipankki-20150115114146.xml"))
    var title = ""
    var intext = false
    var text = ""
    while (xml.hasNext) xml.next match {
      case EvElemStart(_,"title", _, _) => 
        title = xml.next.asInstanceOf[EvText].text.replaceFirst("^Oikeustiede:","").trim
      case EvElemStart(_,"text", _, _) =>
        intext = true
        text+= xml.next.asInstanceOf[EvText].text
      case EvElemEnd(_,"text") =>
        var depth = 0
        var i = 0
        var cs : Seq[String] = Seq.empty
        var cf : Seq[String] = Seq.empty
        var ct = ""
        var form = ""
        while (i<text.length) {
          if (text(i)=='{' && i+1<text.length && text(i+1)=='{') {
           cs = cs :+ ct
           cf = cf :+ form
           ct=""
           depth+=1
           i+=2
           form = ""
           while(text(i)!='\n' && text(i)!='|' && text(i)!='}') {
             form+=text(i)
             i+=1
           }
           i-=1
          } else if (text(i)=='}' && i+1<text.length && text(i+1)=='}') {
            val dct = ct
            if (depth==1) process(form,title,ct)
            ct = cs.last
            if (depth>1) ct+=process2(form,dct)
            form = cf.last
            depth-=1
            cs = cs.dropRight(1)
            i+=1
          } else ct+=text(i)
          i+=1
        }
        text=""
        intext=false
      case EvText(t) if (intext) =>  text+=t
      case er: EvEntityRef if (intext) => XhtmlEntities.entMap.get(er.entity) match {
        case Some(chr) => text+=chr
        case _                              => text+=er.entity
      }
      case _ =>
    }
    RDFDataMgr.write(new FileOutputStream("ttp.nt"), m, RDFFormat.NT)
  }
}
