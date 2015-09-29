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
import scala.collection.mutable.Stack

object FinlexXML2RDF extends Anything2RDF {

  val sns = "http://ldf.fi/finlex-schema#"
  val ns = "http://ldf.fi/finlex/"
  
  val Statute = EC("Statute")
  val Part = EC("Part")
  val Chapter = EC("Chapter")
  val Subheading = EC("Subheadig")
  val Section = EC("Section")
  val Paragraph = EC("Paragraph")
  val Subparagraph = EC("Subparagraph")
  
  
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
      val hbuf = new Stack[Resource]
      val otbuf = new Stack[String]
      otbuf.push(ot)
      hbuf.push(h)
      while (xml.hasNext) xml.next match {
         case EvElemStart(_,"sd", attrs, _) => h=I(ns+toId(attrs("id")(0).text),Statute);
         case EvElemStart(_,"os", attrs, _) =>
           if(!nu.isEmpty) {
             h.addProperty(RDFS.label,nu,"fi")
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
           if(!nu.isEmpty) {
             h.addProperty(RDFS.label,nu,"fi")
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
           if(!nu.isEmpty) {
             h.addProperty(RDFS.label,nu,"fi")
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
             if(!nu.isEmpty) {
               h.addProperty(RDFS.label,nu,"fi")
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
           if(!nu.isEmpty) {
             h.addProperty(RDFS.label,nu,"fi")
             otbuf.push(ot)
             ot+=", "+nu
             nu=""
             h.addProperty(SKOS.prefLabel,ot,"fi")
           }                      
           hbuf.push(h)
           otbuf.push(ot)
           val mid = attrs("id")(0).text
           ot+=", "+mid.replaceFirst(".*:M", "")+" mom."
           c=I(ns+toId(mid),Section)
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
           h.addProperty(SKOS.prefLabel,ot,"fi")
         case EvElemStart(_,"ot", _, _) =>
           var n = xml.next
           if (n.isInstanceOf[EvElemStart] && (n.asInstanceOf[EvElemStart].label=="i" || n.asInstanceOf[EvElemStart].label=="pl")) n=xml.next
           if (n.isInstanceOf[EvText]) {
             val mot= (if (!nu.isEmpty) nu+": "+n.asInstanceOf[EvText].text else n.asInstanceOf[EvText].text).trim 
             h.addProperty(RDFS.label,mot,"fi")
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
               nu=""
               h.addProperty(SKOS.prefLabel,ot,"fi")
             }           
             if(!t.trim.isEmpty) h.addProperty(SIOC.content,t.trim,"fi")
             t=""
             h=hbuf.pop()
             ot=otbuf.pop();
           case _ => 
         } 
         case _ => 
      }
    }
    RDFDataMgr.write(new FileOutputStream("ajantasa.nt"), m, RDFFormat.NT)
  }
}
