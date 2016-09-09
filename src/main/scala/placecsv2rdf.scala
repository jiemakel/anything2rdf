import com.bizo.mighty.csv.CSVReader
import java.net.URLEncoder
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
import org.apache.jena.sparql.vocabulary.FOAF
import com.github.nscala_time.time.Imports._
import org.joda.time.format.ISODateTimeFormat
import org.apache.jena.shared.PrefixMapping
import org.apache.jena.datatypes.xsd.XSDDatatype
import com.bizo.mighty.csv.CSVReaderSettings
import com.bizo.mighty.csv.CSVDictReader

object PLACECSV2RDF extends Anything2RDF {
  
  val sns = "http://ldf.fi/place-schema#"
  val ns = "http://ldf.fi/place/"
  
  def main(args: Array[String]): Unit = {
    var wr = CSVDictReader("placeauthority.tsv")(CSVReaderSettings.Standard.copy(separator='\t'))
    for (r <- wr) {
      val placeType = r("Feature Type") match {
        case "Town/City" => PROCOPECSV2RDF.City
        case "Country" => PROCOPECSV2RDF.Country 
        case "State/Region/Province" => PROCOPECSV2RDF.Province 
        case "Building" => PROCOPECSV2RDF.Building
        case "Park" => PROCOPECSV2RDF.Park
        case "" => CIDOC.Place
        case _ => throw new IllegalArgumentException("Unknown place type: "+r("Feature Type"))
      }
      val place = I(PROCOPECSV2RDF.ns+"location_"+encode(r("Place")),r("Place"),placeType)
      val city = if (!r("City").isEmpty) Some(I(PROCOPECSV2RDF.ns+"location_"+encode(r("City")),r("City"),PROCOPECSV2RDF.City)) else None
      city.foreach(r => place.addProperty(CIDOC.place_falls_within,r))
      val region = if (!r("State/Region/Province").isEmpty) Some(I(PROCOPECSV2RDF.ns+"location_"+encode(r("State/Region/Province")),r("State/Region/Province"),PROCOPECSV2RDF.Province)) else None
      region.foreach(r => city.getOrElse(place).addProperty(CIDOC.place_falls_within,r))
      val country = if (!r("Country").isEmpty) Some(I(PROCOPECSV2RDF.ns+"location_"+encode(r("Country")),r("Country"),PROCOPECSV2RDF.Country)) else None
      country.foreach(c => region.orElse(city).getOrElse(place).addProperty(CIDOC.place_falls_within,c))
      if (!r("GN LatLon").isEmpty) {
        val coords = r("GN LatLon").split(",")
        if (coords.length==2) {
          place.addProperty(WGS84.lat,coords(0).trim)
          place.addProperty(WGS84.long,coords(1).trim)
        }
      }
    }
    wr = CSVDictReader("ee_placeauthority.tsv")(CSVReaderSettings.Standard.copy(separator='\t'))
    for (r <- wr) {
      val placeType = r("GeoNames Feature Code") match {
        case "Town/City" => PROCOPECSV2RDF.City
        case "Country" => PROCOPECSV2RDF.Country 
        case "State/Region/Province" => PROCOPECSV2RDF.Province 
        case "Building" => PROCOPECSV2RDF.Building
        case "Park" => PROCOPECSV2RDF.Park
        case "" => CIDOC.Place
        case _ => throw new IllegalArgumentException("Unknown place type: "+r("Feature Type"))
      }
      val place = I(PROCOPECSV2RDF.ns+"location_"+encode(r("Place")),r("Place"),placeType)
      val city = if (!r("City").isEmpty) Some(I(PROCOPECSV2RDF.ns+"location_"+encode(r("City")),r("City"),PROCOPECSV2RDF.City)) else None
      city.foreach(r => place.addProperty(CIDOC.place_falls_within,r))
      val region = if (!r("State/Region/Province").isEmpty) Some(I(PROCOPECSV2RDF.ns+"location_"+encode(r("State/Region/Province")),r("State/Region/Province"),PROCOPECSV2RDF.Province)) else None
      region.foreach(r => city.getOrElse(place).addProperty(CIDOC.place_falls_within,r))
      val country = if (!r("Country").isEmpty) Some(I(PROCOPECSV2RDF.ns+"location_"+encode(r("Country")),r("Country"),PROCOPECSV2RDF.Country)) else None
      country.foreach(c => region.orElse(city).getOrElse(place).addProperty(CIDOC.place_falls_within,c))
      if (!r("GN LatLon").isEmpty) {
        val coords = r("GN LatLon").split(",")
        if (coords.length==2) {
          place.addProperty(WGS84.lat,coords(0).trim)
          place.addProperty(WGS84.long,coords(1).trim)
        }
      }
/*
 * csv2rdf List(MRofL Place ID, Place, City, State/Region/Province, Country, ISO Country Code, Feature Type, Approximate, Exact place found in GeoNames?, GN ID, GN LatLon, Label for viz, wikipedia link, image uri)
csv2rdf Map(State/Region/Province -> North Rhine-Westphalia, Feature Type -> Town/City, 
Country -> Germany, 
GN LatLon -> 50.77664, 6.08342, Label for viz -> Aachen, GN ID -> 3247449, 
Approximate -> No, Exact place found in GeoNames? -> Yes, 
City -> Aachen, wikipedia link -> , image uri -> , 
ISO Country Code -> DE, Place -> Aachen, MRofL Place ID -> 30001)      
 */
    }
    m.setNsPrefixes(PrefixMapping.Standard)
    m.setNsPrefix("crm",CIDOC.ns)
    m.setNsPrefix("skos",SKOS.ns)
    m.setNsPrefix("foaf",FOAF.NS)
    m.setNsPrefix("place",ns)
    m.setNsPrefix("places",sns)
    m.setNsPrefix("procope",PROCOPECSV2RDF.ns)
    m.setNsPrefix("procopes",PROCOPECSV2RDF.sns)
    RDFDataMgr.write(new FileOutputStream("placeauthority.ttl"), m, RDFFormat.TTL)
  }
}
