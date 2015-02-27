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
import com.bizo.mighty.csv.CSVDictReader
import scala.io.Source
import scala.xml.pull.XMLEventReader
import scala.xml.pull.EvElemStart
import scala.xml.pull.EvElemEnd
import scala.xml.pull.EvText
import scala.xml.pull.EvText
import com.hp.hpl.jena.vocabulary.DCTerms
import scala.collection.mutable.HashMap

object FINLEXKW2RDF extends Anything2RDF {
  
  val sns = "http://ldf.fi/finlex/schema#"
  val ns = "http://ldf.fi/finlex/"

  def main(args: Array[String]): Unit = {
    var xml = new XMLEventReader(Source.fromFile("20140127a_suomi.xml"))
    var id = "" 
    var snimi = ""
    var r : Resource = null
    val fmap = new HashMap[String,Resource]
    val smap = new HashMap[String,Resource]
    while (xml.hasNext) xml.next match { 
      case EvElemStart(_,"asiasana", attrs, _) => id=attrs("id")(0).text
      case EvElemStart(_,"sana", _, _) =>
        val w = xml.next.asInstanceOf[EvText].text
        r = I(ns+"kw"+id,Map("fi"->w),SKOS.Concept)
        fmap.put(w,r)
      case EvElemStart(_,"nimike",_,_) => snimi = xml.next.asInstanceOf[EvText].text 
      case EvElemStart(_,"id", _, _) => 
        val l = m.createResource("http://ldf.fi/finlex/laki/statute-sd"+xml.next.asInstanceOf[EvText].text)
        l.addProperty(SKOS.prefLabel,snimi,"fi")
        l.addProperty(DCTerms.subject,r)
      case _ =>
    }
    xml = new XMLEventReader(Source.fromFile("20140127a_ruotsi.xml"))
    while (xml.hasNext) xml.next match { 
      case EvElemStart(_,"asiasana", attrs, _) => id=attrs("id")(0).text
      case EvElemStart(_,"sana", _, _) =>
        val w = xml.next.asInstanceOf[EvText].text
        r = I(ns+"kw"+id,Map("sv"->w),SKOS.Concept)
        smap.put(w,r)
      case EvElemStart(_,"nimike",_,_) => snimi = xml.next.asInstanceOf[EvText].text 
      case EvElemStart(_,"id", _, _) => 
        val l = m.createResource("http://ldf.fi/finlex/laki/statute-sd"+xml.next.asInstanceOf[EvText].text)
        l.addProperty(SKOS.prefLabel,snimi,"sv")
        l.addProperty(DCTerms.subject,r)
      case _ =>
    }
    var wr = CSVDictReader("Finlex-asiasanat-ks.csv")
    for (r <- wr) {
      if (!r("Suomeksi").isEmpty)
        fmap.getOrElse(r("Suomeksi"), I(ns+"kw_fi_"+encode(r("Suomeksi")),Map("fi"->r("Suomeksi")),SKOS.Concept)).addProperty(SKOS.related,fmap.getOrElse(r("Ks."), I(ns+"kw_fi_"+encode(r("Ks.")),Map("fi"->r("Ks.")),SKOS.Concept)))
      if (!r("Ruotsiksi").isEmpty)
        fmap.getOrElse(r("Ruotsiksi"), I(ns+"kw_sv_"+encode(r("Ruotsiksi")),Map("sv"->r("Ruotsiksi")),SKOS.Concept)).addProperty(SKOS.related,fmap.getOrElse(r("Se."), I(ns+"kw_sv_"+encode(r("Se.")),Map("sv"->r("Se.")),SKOS.Concept)))
    }
    wr = CSVDictReader("Finlex-asiasanat-ksmyos.csv")
    for (r <- wr) {
      if (!r("Suomeksi").isEmpty)
        fmap.getOrElse(r("Suomeksi"), I(ns+"kw_fi_"+encode(r("Suomeksi")),Map("fi"->r("Suomeksi")),SKOS.Concept)).addProperty(SKOS.related,fmap.getOrElse(r("Ks. Myös"), I(ns+"kw_fi_"+encode(r("Ks. Myös")),Map("fi"->r("Ks. Myös")),SKOS.Concept)))
      if (!r("Ruotsiksi").isEmpty)
        fmap.getOrElse(r("Ruotsiksi"), I(ns+"kw_sv_"+encode(r("Ruotsiksi")),Map("sv"->r("Ruotsiksi")),SKOS.Concept)).addProperty(SKOS.related,fmap.getOrElse(r("Se. Även"), I(ns+"kw_sv_"+encode(r("Se. Även")),Map("sv"->r("Se. Även")),SKOS.Concept)))
    }
    wr = CSVDictReader("Finlex-asiasanat-ruotsiksi.csv")
    for (r <- wr) {
      fmap.getOrElse(r("Suomeksi"), I(ns+"kw_fi_"+encode(r("Suomeksi")),Map("fi"->r("Suomeksi")),SKOS.Concept)).addProperty(SKOS.related,fmap.getOrElse(r("Useat käännökset"), I(ns+"kw_sv_"+encode(r("Useat käännökset")),Map("sv"->r("Useat käännökset")),SKOS.Concept)))
      fmap.getOrElse(r("Ruotsiksi"), I(ns+"kw_sv_"+encode(r("Ruotsiksi")),Map("sv"->r("Ruotsiksi")),SKOS.Concept)).addProperty(SKOS.related,fmap.getOrElse(r("Useat käännökset"), I(ns+"kw_sv_"+encode(r("Useat käännökset")),Map("sv"->r("Useat käännökset")),SKOS.Concept)))
    }
    RDFDataMgr.write(new FileOutputStream("finlexkw.nt"), m, RDFFormat.NT)
  }
}
