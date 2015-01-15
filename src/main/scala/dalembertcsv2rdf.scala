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
import com.bizo.mighty.csv.CSVReaderSettings

object DALEMBERTCSV2RDF extends CSV2RDF {
  
  val sns = "http://ldf.fi/dalembert-schema#"
  val ns = "http://ldf.fi/dalembert/"

  val sourceLocationText = EDP("source location text")
  val destLocationText = EOP("destination location text")
  
  def main(args: Array[String]): Unit = {
    val wr = CSVDictReader("dalembert.tsv")(CSVReaderSettings.Standard.copy(separator='\t'))
    for (r <- wr) {
      val dateText = if (r.contains("DateRaw") && !r("DateRaw").isEmpty()) r("DateRaw") else {
         s"${if (!r("DateMonth").isEmpty()) r("DateMonth")+"/" else ""}${if (!r("DateDay").isEmpty()) r("DateDay")+"/" else ""}${if (!r("DateYear").isEmpty()) r("DateYear") else ""}" 
      }
      val letter = I(ns+r("SourceArchiveID").substring(1,r("SourceArchiveID").length-1),s"Letter from ${r("Author")} to ${r("Recipient")}${(if (!dateText.isEmpty()) " on "+ dateText else "")}",EECSV2RDF.Letter)
      if (!r("DateYear").isEmpty()) {
        val (bdateTime,edateTime) = makeDateTime(r("DateYear"),r("DateMonth"),r("DateDay"))        
        val date = I(s"${PROCOPECSV2RDF.ns}date_${bdateTime}TO${edateTime}",dateText,CIDOC.TimeSpan)
      }
      val author = I(ns+encode(r("Author").trim),r("Author").trim,CIDOC.Person)
      letter.addProperty(EECSV2RDF.authorP,author)
      val sourceCountry = if (!r("SourceCountry").isEmpty()) Some(I(PROCOPECSV2RDF.ns+"location_"+encode(r("SourceCountry").trim),r("SourceCountry").trim,PROCOPECSV2RDF.Country)) else None
      val sourceProvince = if (r.contains("SourceRegion") && !r("SourceRegion").isEmpty()) Some(I(PROCOPECSV2RDF.ns+"location_"+encode(r("SourceRegion").trim),r("SourceRegion").trim,PROCOPECSV2RDF.Province)) else None
      sourceProvince.foreach(p => sourceCountry.foreach(c => p.addProperty(CIDOC.place_falls_within,c)))
      val sourceCity = if (!r("SourceCity").isEmpty()) Some(
         if (!r("SourceCountry").isEmpty()) I(PROCOPECSV2RDF.ns+"location_"+encode(r("SourceCity").trim),r("SourceCity").trim,PROCOPECSV2RDF.City) else I(PROCOPECSV2RDF.ns+"location_"+encode(r("SourceCity").trim),r("SourceCity").trim,PROCOPECSV2RDF.Country)
      ) else None
      sourceCity.foreach(city => sourceProvince.orElse(sourceCountry).foreach(sp => city.addProperty(CIDOC.place_falls_within,sp)))
      sourceCity.orElse(sourceProvince.orElse(sourceCountry)).foreach(p => {
        letter.addProperty(EECSV2RDF.sourceLocation,p)        
        val coords = r("SourceLatLon").split(",");
        if (coords.length==2) {
          p.addProperty(WGS84.lat,coords(0).substring(1).trim)
          p.addProperty(WGS84.long,coords(1).substring(0,coords(1).length-1).trim)
        }
      })
      if (!r("SourceRaw").isEmpty()) letter.addProperty(sourceLocationText,r("SourceRaw").substring(1,r("SourceRaw").length-1))
      val recipient = I(ns+encode(r("Recipient").trim),r("Recipient").trim,CIDOC.Person)
      letter.addProperty(EECSV2RDF.recipientP,recipient)
      val destCountry = if (r.contains("DestinationCountry") && !r("DestinationCountry").isEmpty()) Some(I(PROCOPECSV2RDF.ns+"location_"+encode(r("DestinationCountry").trim),r("DestinationCountry").trim,PROCOPECSV2RDF.Country)) else None
      val destProvince = if (r.contains("DestinationRegion") && !r("DestinationRegion").isEmpty()) Some(I(PROCOPECSV2RDF.ns+"location_"+encode(r("DestinationRegion").trim),r("DestinationRegion").trim,PROCOPECSV2RDF.Province)) else None
      destProvince.foreach(p => destCountry.foreach(c => p.addProperty(CIDOC.place_falls_within,c)))
      val destCity = if (r.contains("DestinationCity") && !r("DestinationCity").isEmpty()) Some(
         if (!r("DestinationCountry").isEmpty()) I(PROCOPECSV2RDF.ns+"location_"+encode(r("DestinationCity").trim),r("DestinationCity").trim,PROCOPECSV2RDF.City) else I(PROCOPECSV2RDF.ns+"location_"+encode(r("DestinationCity").trim),r("DestinationCity").trim,PROCOPECSV2RDF.Country)
      ) else None
      destCity.foreach(city => destProvince.orElse(destCountry).foreach(sp => city.addProperty(CIDOC.place_falls_within,sp)))
      destCity.orElse(destProvince.orElse(destCountry)).foreach(p => {
        letter.addProperty(EECSV2RDF.destLocation,p)        
        val coords = r("DestinationLatLon").split(",");
        if (coords.length==2) {
          p.addProperty(WGS84.lat,coords(0).substring(1).trim)
          p.addProperty(WGS84.long,coords(1).substring(0,coords(1).length-1).trim)
        }
      })
      if (r.contains("DestinationRaw") && !r("DestinationRaw").isEmpty()) letter.addProperty(destLocationText,r("DestinationRaw").substring(1,r("DestinationRaw").length-1))
    }
    m.setNsPrefixes(PrefixMapping.Standard)
    m.setNsPrefix("crm",CIDOC.ns)
    m.setNsPrefix("skos",SKOS.ns)
    m.setNsPrefix("foaf",FOAF.NS)
    m.setNsPrefix("da",ns)
    m.setNsPrefix("das",sns)
    m.setNsPrefix("ee",EECSV2RDF.ns)
    m.setNsPrefix("ees",EECSV2RDF.sns)
    m.setNsPrefix("procope",PROCOPECSV2RDF.ns)
    m.setNsPrefix("procopes",PROCOPECSV2RDF.sns)
    RDFDataMgr.write(new FileOutputStream("dalembert.ttl"), m, RDFFormat.TTL)
  }
}
