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
import scala.collection.mutable.HashMap
import com.bizo.mighty.csv.CSVReaderSettings
import org.apache.jena.riot.RIOT

object GeoNamesCSV2RDF extends Anything2RDF {
  
  val sns = "http://www.geonames.org/ontology#"
  val ns = "http://sws.geonames.org/"
  
  val name = P(sns+"name")
  val alternateName = P(sns+"alternateName")
  val featureClass = P(sns+"featureClass")
  val featureCode = P(sns+"featureCode")
  val countryCode = P(sns+"countryCode")
  val parentFeature = P(sns+"parentFeature")
  val parentCountry = P(sns+"parentCountry")
  val alternateParentCountry = P(sns+"alternateParentCountry")
  val parentADM1 = P(sns+"parentADM1")
  val parentADM2 = P(sns+"parentADM2")
  val parentADM3 = P(sns+"parentADM3")
  val parentADM4 = P(sns+"parentADM4")
  val population = P(sns+"population")
  val postalCode = P(sns+"postalCode")
  val elevation = P(sns+"elevation")
  val timezone = P(sns+"timezone")
  
  def main(args: Array[String]): Unit = {
    var dwr = CSVDictReader("countryInfo.txt")(CSVReaderSettings.Standard.copy(separator='\t'))
    val cmap = new HashMap[String,Resource]
    for (r <- dwr)
      cmap.put(r("ISO"),R(ns+r("geonameid")+"/"))
    var wr = CSVReader("admin1CodesASCII.txt")(CSVReaderSettings.Standard.copy(separator='\t')) 
    val a1map = new HashMap[String,Resource]
    for (r <- wr)
      a1map.put(r(0),R(ns+r(3)+"/"))
    m.setNsPrefixes(PrefixMapping.Standard)
    m.setNsPrefix("gn",ns)
    m.setNsPrefix("gns",sns)
    m.setNsPrefix("wgs84",WGS84.ns)
/*    wr = CSVReader("alternateNames.txt")(CSVReaderSettings.Standard.copy(separator='\t',quotechar=null.asInstanceOf[Char]))
    for (r <- wr) {
      val item = R(ns+r(1)+"/")
      if (r(2)=="link") item.addProperty(RDFS.seeAlso,R(r(3)))
      else if (r(2)=="post") item.addProperty(postalCode,r(3))
      else item.addProperty(alternateName,r(3),r(2))
    }
    RDFDataMgr.write(new FileOutputStream("geonames-alternates.nt"), m, RDFFormat.NT)
    m.removeAll()*/
    println("1")
    wr = CSVReader("allCountries.txt")(CSVReaderSettings.Standard.copy(separator='\t',quotechar=null.asInstanceOf[Char]))
    /*
     * geonameid         : integer id of record in geonames database
name              : name of geographical point (utf8) varchar(200)
x asciiname         : name of geographical point in plain ascii characters, varchar(200)
x alternatenames    : alternatenames, comma separated, ascii names automatically transliterated, convenience attribute from alternatename table, varchar(10000)
latitude          : latitude in decimal degrees (wgs84)
longitude         : longitude in decimal degrees (wgs84)
feature class     : see http://www.geonames.org/export/codes.html, char(1)
feature code      : see http://www.geonames.org/export/codes.html, varchar(10)
country code      : ISO-3166 2-letter country code, 2 characters
x cc2               : alternate country codes, comma separated, ISO-3166 2-letter country code, 60 characters
admin1 code       : fipscode (subject to change to iso code), see exceptions below, see file admin1Codes.txt for display names of this code; varchar(20)
admin2 code       : code for the second administrative division, a county in the US, see file admin2Codes.txt; varchar(80) 
admin3 code       : code for third level administrative division, varchar(20)
admin4 code       : code for fourth level administrative division, varchar(20)
population        : bigint (8 byte int) 
elevation         : in meters, integer
x dem               : digital elevation model, srtm3 or gtopo30, average elevation of 3''x3'' (ca 90mx90m) or 30''x30'' (ca 900mx900m) area in meters, integer. srtm processed by cgiar/ciat.
timezone          : the timezone id (see file timeZone.txt) varchar(40)
x modification date : date of last modification in yyyy-MM-dd format
     * 
     */
    for (r <- wr) {
      val item = R(ns+r(0)+"/")
       item.addProperty(name,r(1))
       item.addProperty(WGS84.lat,r(4),XSDDatatype.XSDdecimal)
       item.addProperty(WGS84.long,r(5),XSDDatatype.XSDdecimal)
       item.addProperty(featureClass,R(sns+r(6)))
       item.addProperty(featureCode,R(sns+r(7)))
       var pf=cmap(r(8))
       item.addProperty(parentCountry,pf)
       for (code <- r(9).split(",").filter(c => c!="" && c!=r(8))) item.addProperty(alternateParentCountry,cmap(code))
       if (r(13)!="") {
         pf = R(ns+r(13)+"/")
         item.addProperty(parentADM4,pf)
       }
       if (r(12)!="") {
         pf = R(ns+r(12)+"/")
         item.addProperty(parentADM3,pf)
       }
       if (r(11)!="") {
         pf = R(ns+r(11)+"/")
         item.addProperty(parentADM2,pf)
       }
       if (r(10)!="00") {
         for (code <- r(9).split(",").filter(_!="")) if (a1map.contains(code+'.'+r(10)))
           pf = a1map(code+'.'+r(10))
         if (a1map.contains(r(8)+'.'+r(10)))
           pf = a1map(r(8)+'.'+r(10))
         item.addProperty(parentADM1,pf)
       }
       item.addProperty(parentFeature,pf)
       if (r(14)!="0") item.addProperty(population, r(14),XSDDatatype.XSDinteger)
       if (r(15)!="") item.addProperty(elevation, r(15),XSDDatatype.XSDinteger)
       item.addLiteral(timezone,r(17))
    }
    RDFDataMgr.write(new FileOutputStream("geonames.ttl"), m, RDFFormat.TTL)
  }
}
