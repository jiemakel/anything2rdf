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

object TTPXML2RDF extends CSV2RDF {

  val sns = "http://ldf.fi/and-schema#"
  val ns = "http://ldf.fi/and/"

  val period = EOP("period")
  val authorial_presence = EDP("authorial presence")
  val mentions = EOP("mentions")
  val genre = EOP("genre")
  val practitioner_type = EOP("practitioner type")
  val prefatory_names = EOP("prefatory names")
  val Work = EC("Work")
  val Person = EC("Person")
  val Genre = EC("Genre")
  val Period = EC("Period")
  val Date = EC("Date")
  val PractitionerType = EC("Practitioner Type")
  val prose = I(sns + "practitionerTypeProse", Map("en" -> "Prose"), PractitionerType)
  val canonical = I(sns + "practitionerTypeCanonical", Map("en" -> "Canonical"), PractitionerType)
  val noncanonical = I(sns + "practitionerTypeNonCanonical", Map("en" -> "Non-Canonical"), PractitionerType)
  val poetry = I(sns + "PractitionerTypePoetry", Map("en" -> "Poetry"), PractitionerType)
  val roman = I(sns + "PractitionerTypeRoman", Map("en" -> "Roman"), PractitionerType)
  val contemporary = I(sns + "PractitionerTypeContemporary", Map("en" -> "Contemporary"), PractitionerType)
  val myth = I(sns + "PractitionerTypeMyth", Map("en" -> "Myth"), PractitionerType)
  val oral = I(sns + "PractitionerTypeOral", Map("en" -> "Oral"), PractitionerType)

  def pmentions(s: String, w: Resource, ptypes: Resource*) = s.trim.split(',').map(s => s.trim).filter(s => !s.isEmpty).foreach(s => {
    val p2 = I(ns + "person_" + encode(s), s, Person)
    w.addProperty(mentions, p2)
    ptypes.foreach(ptype => p2.addProperty(practitioner_type, ptype))
  })
  
  private val fields = """(?s)\|(.*?)=([^\|]+)""".r

  def process(form : String, title : String, content : String) : Unit = {
    var ct = content
    ct = ct.replaceAll("""\[\[.*?\|(.*)\]\]""", """\1""")
    ct = ct.replaceAll("""\[\[Oikeustiede:(.*?)\]\]""","""\1""")
    ct = ct.replaceAll("""\[\[(.*?)\]\]""", """\1""")
    val r = I(ns+"_"+encode(title),Map("fi"->title),SKOS.Concept)
    form match {
      case "Käsite" =>
        for (mt <- fields.findAllMatchIn(ct)) {
          val tag = mt.group(1).toLowerCase()
          val content = mt.group(2)
          tag match {
            case "selite_fi" => r.addProperty(RDFS.comment,content,"fi")
            case "määritelmä_fi" => r.addProperty(DCTerms.description,content,"fi")
            case "logotiedosto" | "logolinkki" | "tarkistettu" =>
            case u => //println(u)
          }
        }            
      case "Lähikäsite" => for (mt <- fields.findAllMatchIn(ct)) {
          val tag = mt.group(1).toLowerCase()
          val content = mt.group(2)
          tag match {
            case "käsite" => 
              val n2 = content.replaceFirst("^Oikeustiede:","")
              r.addProperty(SKOS.related,I(ns+"_"+encode(n2),Map("fi"->n2),SKOS.Concept))
            case u => //println(u)
          }
      }
      case "Liittyvä nimitys" => for (mt <- fields.findAllMatchIn(ct)) {
          val tag = mt.group(1).toLowerCase()
          val content = mt.group(2)
          val lang = ""
          tag match {
            case "kieli" => 
            case "nimitys" => 
            case u => //println(u)
          }
      }
      case u => println("Unknown form: "+u)      
    }
    
  }
  
  def process2(form : String, content : String) : String = {
    if (form.toLowerCase()=="lakilinkki|") content.substring(4)+"/"+content.substring(0,4)
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
        title = xml.next.asInstanceOf[EvText].text.replaceFirst("^Oikeustiede:","")
      case EvElemStart(_,"text", _, _) =>
        intext = true
        text+= xml.next.asInstanceOf[EvText].text
      case EvElemEnd(_,"text") =>
        var depth = 0
        var i = 0
        var cs : Seq[String] = Seq.empty
        var ct = ""
        var form = ""
        while (i<text.length) {
          if (text(i)=='{' && i+1<text.length && text(i+1)=='{') {
           cs = cs :+ ct
           ct=""
           depth+=1
           i+=2
           form = ""
           while(text(i)!='\n' && text(i)!='|' && text(i)!='}') {
             form+=text(i)
             i+=1
           }
           i-=2
          } else if (text(i)=='}' && i+1<text.length && text(i+1)=='}') {
            val dct = ct
            if (depth==1) process(form,title,ct)
            ct = cs.last
            if (depth>1) ct+=process2(form,dct)
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
