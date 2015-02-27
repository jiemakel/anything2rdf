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
import com.bizo.mighty.csv.CSVDictReader

object WIKIPEDIAWW1EVENTSCSV2RDF extends Anything2RDF {
  
  val sns = "http://ldf.fi/ww1lod/schema#"
  val ns = "http://ldf.fi/ww1lod/"
  
  def main(args: Array[String]): Unit = {
    var wr = CSVDictReader("WWI-events-wikipedia-links.csv")
    var i = 1
    for (r <- wr) {
       val event = I(ns+"we"+i,Map("en"->r("Event-no-links")),CIDOC.Event)
       val theme = I(ns+"wet_"+r("Theater"),Map("en"->r("Theater")),ResourceFactory.createResource(sns+"Theme"))
       event.addProperty(CIDOC.shows_features_of,theme)
       if (!r("details_link").isEmpty) event.addProperty(FOAF.page,ResourceFactory.createResource(r("details_link")))
       val bdate = r("Begin").trim
       val edate = r("End").trim
       val ts = if (edate.isEmpty()) {
         val ts = I(ns+s"timespan_${bdate}",bdate,CIDOC.TimeSpan)
         ts.addProperty(CIDOC.begin_of_the_begin, bdate+"T00:00:00.000",XSDDatatype.XSDdateTime)
         ts.addProperty(CIDOC.end_of_the_end, bdate+"T23:59:59.999",XSDDatatype.XSDdateTime)
         ts
       } else {
         val ts = I(ns+s"timespan_${bdate}TO${edate}",s"${bdate}-${edate}",CIDOC.TimeSpan)
         ts.addProperty(CIDOC.begin_of_the_begin, bdate+"T00:00:00.000",XSDDatatype.XSDdateTime)
         ts.addProperty(CIDOC.end_of_the_end, edate+"T23:59:59.999",XSDDatatype.XSDdateTime)
         ts
       }
       event.addProperty(CIDOC.has_timeSpan,ts)
      i+=1
    }
    m.setNsPrefixes(PrefixMapping.Standard)
    m.setNsPrefix("crm",CIDOC.ns)
    m.setNsPrefix("skos",SKOS.ns)
    m.setNsPrefix("ww1l",ns)
    m.setNsPrefix("ww1s",sns)
    RDFDataMgr.write(new FileOutputStream("wikipediaww1events.ttl"), m, RDFFormat.TTL)
  }
}
