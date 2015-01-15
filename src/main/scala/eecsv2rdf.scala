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

object EECSV2RDF extends CSV2RDF {
  
  val sns = "http://ldf.fi/ee-schema#"
  val ns = "http://ldf.fi/ee/"

  val Letter = EC("Letter").addProperty(RDFS.subClassOf, CIDOC.Physical_ManMade_Thing)
  
  
  val sourceLocation = EOP("source location")
  val via = EDP("via")
  val viaLocation = EOP("via location")
  val forwardedLocation = EOP("forwarded location")
  val destLocation = EOP("destination location")
  val possibleDate = EOP("has possible time-span")
  val authorP = EOP("author")
  val recipientP = EOP("recipient")
  possibleDate.addProperty(RDFS.subPropertyOf, CIDOC.has_timeSpan)
  
  def main(args: Array[String]): Unit = {
    val wr = CSVDictReader("ee.tsv")(CSVReaderSettings.Standard.copy(separator='\t'))
    for (r <- wr) {
      val label = "Letter from " + r("Author names") + " to " + r("Recipient names") + (if (!r("letdoc_FullDate_WithDay").isEmpty()) " on "+ r("letdoc_FullDate_WithDay") else "")
      val letter = I(ns+"letter_"+encode(r("letdocID")),Map("en"->label),Letter)
      val author = I(ns+"person_"+encode(r("Author perID")),Map("en"->r("Author names")),CIDOC.Person)
      if(!r("Author first name").isEmpty()) author.addProperty(FOAF.firstName,r("Author first name"))
      if(!r("Author last name").isEmpty()) author.addProperty(FOAF.family_name,r("Author last name"))
      letter.addProperty(authorP,author)
      val srcCountry = if (!r("src_country").isEmpty()) Some(I(PROCOPECSV2RDF.ns+"location_"+encode(r("src_country")),Map("en"->r("src_country")),PROCOPECSV2RDF.Country)) else None
      val srcProvince = if (!r("src_province").isEmpty()) Some(I(PROCOPECSV2RDF.ns+"location_"+encode(r("src_province")),Map("en"->r("src_province")),PROCOPECSV2RDF.Province)) else None
      srcProvince.foreach(p => srcCountry.foreach(c => p.addProperty(CIDOC.place_falls_within,c)))
      val srcCity = if (!r("src_city").isEmpty()) Some(
         if (!r("src_country").isEmpty()) I(PROCOPECSV2RDF.ns+"location_"+encode(r("src_city")),Map("en"->r("src_city")),PROCOPECSV2RDF.City) else I(PROCOPECSV2RDF.ns+"location_"+encode(r("src_city")),Map("en"->r("src_city")),PROCOPECSV2RDF.Country)
      ) else None
      srcCity.foreach(city => srcProvince.orElse(srcCountry).foreach(sp => city.addProperty(CIDOC.place_falls_within,sp)))
      srcCity.orElse(srcProvince.orElse(srcCountry)).foreach(p => letter.addProperty(sourceLocation,p))
      val viaCountry = if (!r("via_country").isEmpty()) Some(I(PROCOPECSV2RDF.ns+"location_"+encode(r("via_country")),Map("en"->r("via_country")),PROCOPECSV2RDF.Country)) else None
      val viaProvince = if (!r("via_province").isEmpty()) Some(I(PROCOPECSV2RDF.ns+"location_"+encode(r("via_province")),Map("en"->r("via_province")),PROCOPECSV2RDF.City)) else None
      viaProvince.foreach(p => viaCountry.foreach(c => p.addProperty(CIDOC.place_falls_within,c)))
      if (!r("via_city").isEmpty()) letter.addProperty(via,r("via_city"))
      viaProvince.orElse(viaCountry).foreach(p => letter.addProperty(viaLocation,p))
      val recipient = I(ns+"person_"+encode(r("Recipient perID")),Map("en"->r("Recipient names")),CIDOC.Person)
      if(!r("Recipient first name").isEmpty()) recipient.addProperty(FOAF.firstName,r("Recipient first name"))
      if(!r("Recipient last name").isEmpty()) recipient.addProperty(FOAF.family_name,r("Recipient last name"))
      letter.addProperty(recipientP,recipient)
      val forwardedCountry = if (!r("forwarded_country").isEmpty()) Some(I(PROCOPECSV2RDF.ns+"location_"+encode(r("forwarded_country")),Map("en"->r("forwarded_country")),PROCOPECSV2RDF.Country)) else None
      val forwardedProvince = if (!r("forwarded_province").isEmpty()) Some(I(PROCOPECSV2RDF.ns+"location_"+encode(r("forwarded_province")),Map("en"->r("forwarded_province")),PROCOPECSV2RDF.Province)) else None
      forwardedProvince.foreach(p => forwardedCountry.foreach(c => p.addProperty(CIDOC.place_falls_within,c)))
      val forwardedCity = if (!r("forwarded_city").isEmpty()) Some(
         if (!r("forwarded_country").isEmpty()) I(PROCOPECSV2RDF.ns+"location_"+encode(r("forwarded_city")),Map("en"->r("forwarded_city")),PROCOPECSV2RDF.City) else I(PROCOPECSV2RDF.ns+"location_"+encode(r("forwarded_city")),Map("en"->r("forwarded_city")),PROCOPECSV2RDF.Country)
      ) else None
      forwardedCity.foreach(city => forwardedProvince.orElse(forwardedCountry).foreach(sp => city.addProperty(CIDOC.place_falls_within,sp)))
      forwardedCity.orElse(forwardedProvince.orElse(forwardedCountry)).foreach(p => letter.addProperty(forwardedLocation,p))
      val destCountry = if (!r("dest_country").isEmpty()) Some(I(PROCOPECSV2RDF.ns+"location_"+encode(r("dest_country")),Map("en"->r("dest_country")),PROCOPECSV2RDF.Country)) else None
      val destProvince = if (!r("dest_province").isEmpty()) Some(I(PROCOPECSV2RDF.ns+"location_"+encode(r("dest_province")),Map("en"->r("dest_province")),PROCOPECSV2RDF.Province)) else None
      destProvince.foreach(p => destCountry.foreach(c => p.addProperty(CIDOC.place_falls_within,c)))
      val destCity = if (!r("dest_city").isEmpty()) Some(
         if (!r("dest_country").isEmpty()) I(PROCOPECSV2RDF.ns+"location_"+encode(r("dest_city")),Map("en"->r("dest_city")),PROCOPECSV2RDF.City) else I(PROCOPECSV2RDF.ns+"location_"+encode(r("dest_city")),Map("en"->r("dest_city")),PROCOPECSV2RDF.Country)
      ) else None
      destCity.foreach(city => destProvince.orElse(destCountry).foreach(sp => city.addProperty(CIDOC.place_falls_within,sp)))
      destCity.orElse(destProvince.orElse(destCountry)).foreach(p => letter.addProperty(destLocation,p))
      if (!r("letdoc_FullDate_WithDay").isEmpty()) {
        val (bdateTime,edateTime) = makeDateTime(r("letdoc_Date_year"),r("letdoc_Date_month"),r("letdoc_Date_date"))
        val date = I(s"${PROCOPECSV2RDF.ns}date_${bdateTime}TO${edateTime}",if (r("letdoc_FullDate_WithDay").startsWith("(possible date)")) r("letdoc_FullDate_WithDay").substring(15).trim else r("letdoc_FullDate_WithDay") ,CIDOC.TimeSpan)
        
        date.addProperty(CIDOC.begin_of_the_begin,bdateTime,XSDDatatype.XSDdateTime)
        date.addProperty(CIDOC.end_of_the_end,edateTime,XSDDatatype.XSDdateTime)
        if (r("letdoc_FullDate_WithDay").startsWith("(possible date)")) letter.addProperty(possibleDate,date)
        else letter.addProperty(CIDOC.has_timeSpan,date)
      }
      if (m.contains(ResourceFactory.createResource(PROCOPECSV2RDF.ns+"location_Paris"), CIDOC.place_falls_within,ResourceFactory.createResource(PROCOPECSV2RDF.ns+"location_England"))) {
        println(r)
        System.exit(-1)
      }
      if (m.contains(ResourceFactory.createResource(PROCOPECSV2RDF.ns+"location_Paris"), CIDOC.place_falls_within,ResourceFactory.createResource(PROCOPECSV2RDF.ns+"location_Switzerland"))) {
        println(r)
        System.exit(-2)
      }
    }    
    m.setNsPrefixes(PrefixMapping.Standard)
    m.setNsPrefix("crm",CIDOC.ns)
    m.setNsPrefix("skos",SKOS.ns)
    m.setNsPrefix("foaf",FOAF.NS)
    m.setNsPrefix("ee",ns)
    m.setNsPrefix("ees",sns)
    m.setNsPrefix("procope",PROCOPECSV2RDF.ns)
    m.setNsPrefix("procopes",PROCOPECSV2RDF.sns)
    RDFDataMgr.write(new FileOutputStream("ee.ttl"), m, RDFFormat.TTL)
  }
}
