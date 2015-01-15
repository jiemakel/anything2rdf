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

object PRINCIPALEVENTSCSV2RDF extends CSV2RDF {
  
  val sns = "http://ldf.fi/ww1lod/schema"
  val ns = "http://ldf.fi/ww1lod/"
  
  def main(args: Array[String]): Unit = {
    var wr = CSVReader("principalevents.csv")(CSVReaderSettings.Standard.copy(separator=';',quotechar='€'))
    var i = 1
    for (w <- wr;s <- w(3).split('.').filter(p => !p.isEmpty())) {
       val event = I(ns+"pe"+i,Map("en"->s.trim),CIDOC.Event)
       val year = w(0).trim
       val month = w(1).trim match {
         case "JANUARY" => "01"
         case "FEBRUARY" => "02"
         case "MARCH" => "03"
         case "APRIL" => "04"
         case "MAY" => "05"
         case "JUNE" => "06"
         case "JULY" => "07"
         case "AUGUST" => "08"
         case "SEPTEMBER" => "09"
         case "OCTOBER" => "10"
         case "NOVEMBER" => "11"
         case "DECEMBER" => "12"
       }
       val date = if (w(2).trim.length==1) "0"+w(2).trim else w(2).trim
       val isodate = s"${year}-${month}-${date}"
       val ts = I(ns+s"timespan_${isodate}",isodate,CIDOC.TimeSpan)
       ts.addProperty(CIDOC.begin_of_the_begin, isodate+"T00:00:00.000",XSDDatatype.XSDdateTime)
       ts.addProperty(CIDOC.end_of_the_end, isodate+"T23:59:59.999",XSDDatatype.XSDdateTime)
       event.addProperty(CIDOC.has_timeSpan,ts)
      i+=1
    }
    m.setNsPrefixes(PrefixMapping.Standard)
    m.setNsPrefix("crm",CIDOC.ns)
    m.setNsPrefix("skos",SKOS.ns)
    m.setNsPrefix("ww1l",ns)
    RDFDataMgr.write(new FileOutputStream("principalevents.ttl"), m, RDFFormat.TTL)
  }
}
