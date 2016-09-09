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

object FinlexXML2RDF extends Anything2RDF {

  val sns = "http://ldf.fi/finlex/schema#"
  val ns = "http://ldf.fi/finlex/"
  
  val Statute = EC("Statute")
  val Part = EC("Part")
  val Chapter = EC("Chapter")
  val Subheading = EC("Subheading")
  val Section = EC("Section")
  val Subsection = EC("Subsection")
  val Paragraph = EC("Paragraph")
  val Subparagraph = EC("Subparagraph")
  
  val completeContent = EDP("Complete content") 
  
  def toId(id : String) : String = {
    id.replaceAll(":", "/")
  }

  def main(args: Array[String]): Unit = {
    for (dir <- new java.io.File("sd/").listFiles;
        file <- dir.listFiles) {
      println("Processing: "+file)
      val xml = new XMLEventReader(Source.fromFile(file))
      var h : Resource = null
      var c : Resource = null
      var ko : String = null
      var nu : String = ""
      var t : String = ""
      var ot : String = ""
      var ft : String = ""
      val hbuf = new Stack[Resource]
      val otbuf = new Stack[String]
      val tbuf = new Stack[String]
      otbuf.push(ot)
      while (xml.hasNext) xml.next match {
         case EvElemStart(_,"sd", attrs, _) =>
           hbuf.push(h)
           tbuf.push(ft)
           ft = ""
           h=I(ns+toId(attrs("id")(0).text),Statute);
         case EvElemStart(_,"os", attrs, _) =>
           tbuf.push(ft)
           ft = ""
           if(!nu.isEmpty) {
             h.addProperty(RDFS.label,nu,"fi")
             ft += nu + "\n"
             otbuf.push(ot)
             ot+=", "+nu
             nu=""
             h.addProperty(SKOS.prefLabel,ot,"fi")
           }           
           hbuf.push(h)
           c=I(ns+toId(attrs("id")(0).text),Part)
           c.addProperty(DCTerms.isPartOf,h)
           h=c
         case EvElemStart(_,"lu", attrs, _) =>
           tbuf.push(ft)
           ft = ""
           if(!nu.isEmpty) {
             h.addProperty(RDFS.label,nu,"fi")
             ft += nu + "\n"
             otbuf.push(ot)
             ot+=", "+nu
             nu=""
             h.addProperty(SKOS.prefLabel,ot,"fi")
           }           
           hbuf.push(h)
           c=I(ns+toId(attrs("id")(0).text),Chapter)
           c.addProperty(DCTerms.isPartOf,h)
           h=c
         case EvElemStart(_,"vo", attrs, _) =>
           tbuf.push(ft)
           ft = ""
           if(!nu.isEmpty) {
             h.addProperty(RDFS.label,nu,"fi")
             ft += nu + "\n"
             otbuf.push(ot)
             ot+=", "+nu
             nu=""
             h.addProperty(SKOS.prefLabel,ot,"fi")
           }                      
           hbuf.push(h)
           c=I(ns+toId(attrs("id")(0).text),Subheading)
           c.addProperty(DCTerms.isPartOf,h)
           h=c
         case EvElemStart(_,"py", attrs, _) =>
           if (attrs("id")==null) {
             var break = false
             while (!break) {
               xml.next match {
                 case EvText(text) => t+=text
                 case EvElemEnd(_,"py") => break=true
                 case _ => 
               }
             } 
           } else {
             tbuf.push(ft)
             ft = ""             
             if(!nu.isEmpty) {
               h.addProperty(RDFS.label,nu,"fi")
               ft += nu + "\n"
               otbuf.push(ot)
               ot+=", "+nu
               nu=""
               h.addProperty(SKOS.prefLabel,ot,"fi")
             }                      
             hbuf.push(h)
             c=I(ns+toId(attrs("id")(0).text),Section);c.addProperty(DCTerms.isPartOf,h)
             h=c
           }
         case EvElemStart(_,"mo", attrs, _) =>
           tbuf.push(ft)
           ft = ""
           if(!nu.isEmpty) {
             h.addProperty(RDFS.label,nu,"fi")
             ft += nu + "\n"
             otbuf.push(ot)
             ot+=", "+nu
             nu=""
             h.addProperty(SKOS.prefLabel,ot,"fi")
           }                      
           hbuf.push(h)
           otbuf.push(ot)
           val mid = attrs("id")(0).text
           ot+=", "+mid.replaceFirst(".*:M", "")+" mom."
           c=I(ns+toId(mid),Subsection)
           c.addProperty(DCTerms.isPartOf,h)
           h=c
         case EvElemStart(_,"ko", attrs, _) =>
           if (attrs("id")==null) { 
             var level=1
             while (level!=0) {
               xml.next match {
                 case EvText(text) => t+=text
                 case EvElemStart(_,"ko",_,_) => level+=1
                 case EvElemEnd(_,"ko") => level-=1
                 case _ => 
               }
             } 
           } else {
             tbuf.push(ft)
             ft = ""
             hbuf.push(h)
             otbuf.push(ot)
             val mid = attrs("id")(0).text
             if (mid.length!=1) { 
               ko = toId(mid);
               c=I(ns+ko,Paragraph)
               ot+=", kohta "+mid.replaceFirst(".*:K", "")
             } else {
               c=I(ns+ko+"/"+mid.toUpperCase,Subparagraph)
               ot+=mid
             }
             c.addProperty(RDFS.label,mid.replaceFirst(".*:K", ""),"fi")
             c.addProperty(SKOS.prefLabel,ot,"fi")
             c.addProperty(DCTerms.isPartOf,h)
             h=c
           }
         case EvText(text) => t+=text
         case EvElemStart(_,"ni", _, _) => 
           ot=xml.next.asInstanceOf[EvText].text
           h.addProperty(RDFS.label,ot,"fi")
           ft += ot + "\n"
           h.addProperty(SKOS.prefLabel,ot,"fi")
         case EvElemStart(_,"ot", _, _) =>
           var n = xml.next
           if (n.isInstanceOf[EvElemStart] && (n.asInstanceOf[EvElemStart].label=="i" || n.asInstanceOf[EvElemStart].label=="pl")) n=xml.next
           if (n.isInstanceOf[EvText]) {
             val mot= (if (!nu.isEmpty) nu+": "+n.asInstanceOf[EvText].text else n.asInstanceOf[EvText].text).trim 
             h.addProperty(RDFS.label,mot,"fi")
             ft += mot + "\n"
             otbuf.push(ot)
             ot+=", "+mot
             h.addProperty(SKOS.prefLabel,ot,"fi")
           } else println("Borked ot: "+n) 
           nu=""
         case EvElemStart(_,"nu", _, _) => 
           var n = xml.next
           if (n.isInstanceOf[EvElemStart] && (n.asInstanceOf[EvElemStart].label=="i" || n.asInstanceOf[EvElemStart].label=="pl")) n=xml.next           
           if (n.isInstanceOf[EvText])
             nu=n.asInstanceOf[EvText].text
           else if (n.isInstanceOf[EvElemEnd]) {
             nu="?"
           } else println("Borked nu: "+n)
         case EvElemEnd(_,elem) => elem match {
           case "sd" |"os" | "lu" | "vo" | "py" | "mo" | "ko" =>
             if(!nu.isEmpty) {
               h.addProperty(RDFS.label,nu,"fi")
               otbuf.push(ot)
               ot+=", "+nu
               ft+=ot+'\n'
               nu=""
               h.addProperty(SKOS.prefLabel,ot,"fi")
             }           
             if(!t.trim.isEmpty) {
               h.addProperty(SIOC.content,t.trim,"fi")
               ft+=t.trim+"\n"
             }
             if (!ft.trim.isEmpty)
               h.addProperty(completeContent,ft,"fi")
             val tmpft = ft
             ft=tbuf.pop()
             ft+=tmpft
             t=""
             h=hbuf.pop()
             ot=otbuf.pop()
           case _ => 
         } 
         case _ => 
      }
    }
    RDFDataMgr.write(new FileOutputStream("ajantasa.nt"), m, RDFFormat.NT)
  }
}
