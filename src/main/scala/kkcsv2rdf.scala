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
import com.bizo.mighty.csv.CSVReaderSettings
import com.bizo.mighty.csv.CSVDictReader

object KKCSV2RDF extends Anything2RDF {
  
  val sns = "http://ldf.fi/fnewspapers-schema#"
  val ns = "http://ldf.fi/fnewspapers/"
  
  def main(args: Array[String]): Unit = {
    var wr = CSVDictReader("kansalliskirjastoslmeta.csv")(CSVReaderSettings.Standard.copy(separator=';'))
    for (r <- wr) {
      val p = R(ns+"paper_"+r("ISSN").trim)
      p.addProperty(FOAF.page,R(r("Nimekkeen kotisivun URL").trim))
      p.addProperty(RDFS.label,r("Nimeke").trim)
      p.addProperty(P("http://seco.hut.fi/u/eeahone/artikkelihakemisto#KIELI"),R("http://www.lingvoj.org/lang/"+r("Kieli (RFC3066)").trim))
      val place = I(ns+"place_"+encode(r("Ilmestymispaikka").trim),r("Ilmestymispaikka").trim,CIDOC.Place)
      val coords = r("Koordinaatit").trim.split(",")
      place.addProperty(WGS84.lat, coords(0).trim, XSDDatatype.XSDdecimal)
      place.addProperty(WGS84.long, coords(1).trim, XSDDatatype.XSDdecimal)
      p.addProperty(P(sns+"ISSN"),r("ISSN"))
      p.addProperty(CIDOC.has_former_or_current_location,place)
      val d1 = r("Ilmestymispvm (dd/mm/yyyy)").trim.split("/")
      val d12 = makeDateTime(d1(2), d1(1), d1(0))
      val d2 = r("Julkaisuloppuu (dd/mm/yyyy)").trim.split("[/\\.]")
      val d22 = makeDateTime(d2(2), d2(1), d2(0))
      p.addProperty(CIDOC.has_timeSpan,makeTimeSpan(d1(2)+"-"+d1(1)+"-"+d1(0)+" -- "+d2(2)+"-"+d2(1)+"-"+d2(0),d12,d22));
    }
    m.setNsPrefixes(PrefixMapping.Standard)
    m.setNsPrefix("crm",CIDOC.ns)
    m.setNsPrefix("skos",SKOS.ns)
    m.setNsPrefix("wgs84",WGS84.ns)
    m.setNsPrefix("fn",ns)
    m.setNsPrefix("fns",sns)
    RDFDataMgr.write(new FileOutputStream("kk.ttl"), m, RDFFormat.TTL)
  }
}
