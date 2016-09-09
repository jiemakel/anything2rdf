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
  
  def process(wr: CSVReader): Unit = {
        /*
geonameid         : integer id of record in geonames database
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
     */
    for (r <- wr) {
      val item = R(ns+r(0)+"/")
       item.addProperty(name,r(1))
       if (r(4)!="") item.addProperty(WGS84.lat,r(4),XSDDatatype.XSDdecimal)
       if (r(5)!="") item.addProperty(WGS84.long,r(5),XSDDatatype.XSDdecimal)
       if (r(6)!="") item.addProperty(featureClass,R(sns+r(6)))
       if (r(7)!="") item.addProperty(featureCode,R(sns+r(7)))
       for (code <- r(9).split(",").filter(c => c!="" && c!=r(8))) item.addProperty(alternateParentCountry,cmap(code))
       var pf = cmap.get(r(8))
       pf.foreach(item.addProperty(parentCountry,_)) // YU (Federal Republic of Yugoslavia) is not in countryInfo.txt. Neither are there countries (by definition) for null.txt 
       if (r(13)!="" && r(13)!="00") {
         pf = Some(R(ns+URLEncoder.encode(r(13),"UTF-8")+"/"))
         for (code <- r(9).split(",").filter(_!="")) if (a4map.contains(code+'.'+r(10)+'.'+r(11)+'.'+r(12)+'.'+r(13)))
           pf = Some(a4map(code+'.'+r(10)+'.'+r(11)+'.'+r(12)+'.'+r(13)))
         if (a4map.contains(r(8)+'.'+r(10)+'.'+r(11)+'.'+r(12)+'.'+r(13)))
           pf = Some(a4map(r(8)+'.'+r(10)+'.'+r(11)+'.'+r(12)+'.'+r(13)))
         item.addProperty(parentADM4,pf.get)
       }
       if (r(12)!="" && r(12)!="00") {
         pf = Some(R(ns+URLEncoder.encode(r(12),"UTF-8")+"/"))
         for (code <- r(9).split(",").filter(_!="")) if (a3map.contains(code+'.'+r(10)+'.'+r(11)+'.'+r(12)))
           pf = Some(a3map(code+'.'+r(10)+'.'+r(11)+'.'+r(12)))
         if (a3map.contains(r(8)+'.'+r(10)+'.'+r(11)+'.'+r(12)))
           pf = Some(a3map(r(8)+'.'+r(10)+'.'+r(11)+'.'+r(12)))
         item.addProperty(parentADM3,pf.get)
       }
       if (r(11)!="" && r(11)!="00") {
         pf = Some(R(ns+URLEncoder.encode(r(11),"UTF-8")+"/"))
         for (code <- r(9).split(",").filter(_!="")) if (a2map.contains(code+'.'+r(10)+'.'+r(11)))
           pf = Some(a2map(code+'.'+r(10)+'.'+r(11)))
         if (a2map.contains(r(8)+'.'+r(10)+'.'+r(11)))
           pf = Some(a2map(r(8)+'.'+r(10)+'.'+r(11)))
         item.addProperty(parentADM2,pf.get)
       }
       if (r(10)!="" && r(10)!="00") {
         pf = Some(R(ns+URLEncoder.encode(r(10),"UTF-8")+"/"))
         for (code <- r(9).split(",").filter(_!="")) if (a1map.contains(code+'.'+r(10)))
           pf = Some(a1map(code+'.'+r(10)))
         if (a1map.contains(r(8)+'.'+r(10)))
           pf = Some(a1map(r(8)+'.'+r(10)))
         pf.foreach(item.addProperty(parentADM1,_))
       }
       pf.foreach(item.addProperty(parentFeature,_))
       if (r(14)!="0") item.addProperty(population, r(14),XSDDatatype.XSDinteger)
       if (r(15)!="") item.addProperty(elevation, r(15),XSDDatatype.XSDinteger)
       item.addProperty(timezone,r(17))
    }
  }
  
  val cmap = new HashMap[String,Resource]
  val a1map = new HashMap[String,Resource]
  val a2map = new HashMap[String,Resource]
  val a3map = new HashMap[String,Resource]
  val a4map = new HashMap[String,Resource]
  
  def main(args: Array[String]): Unit = {
    var dwr = CSVDictReader("countryInfo.txt")(CSVReaderSettings.Standard.copy(separator='\t'))
    for (r <- dwr)
      cmap.put(r("ISO"),R(ns+r("geonameid")+"/"))
    var wr = CSVReader("admin1Codes.txt")(CSVReaderSettings.Standard.copy(separator='\t')) // cut allCountries.txt -f 1,8,9,10,11 | grep '\bADM1\b' > admin1Codes.txt
    for (r <- wr) {
      for (code <- r(3).split(",").filter(_!=""))
        a1map.put(code+'.'+r(4),R(ns+r(0)+"/"))
      a1map.put(r(2)+'.'+r(4),R(ns+r(0)+"/"))      
    }
    wr = CSVReader("admin2Codes.txt")(CSVReaderSettings.Standard.copy(separator='\t')) // cut allCountries.txt -f 1,8,9,10,11,12 | grep '\bADM2\b' > admin2Codes.txt
    for (r <- wr) {
      for (code <- r(3).split(",").filter(_!=""))
        a2map.put(code+'.'+r(4)+'.'+r(5),R(ns+r(0)+"/"))
      a2map.put(r(2)+'.'+r(4)+'.'+r(5),R(ns+r(0)+"/"))      
    }
    wr = CSVReader("admin3Codes.txt")(CSVReaderSettings.Standard.copy(separator='\t')) // cut allCountries.txt -f 1,8,9,10,11,12,13 | grep '\bADM3\b' > admin3Codes.txt
    for (r <- wr) {
      for (code <- r(3).split(",").filter(_!=""))
        a3map.put(code+'.'+r(4)+'.'+r(5)+'.'+r(6),R(ns+r(0)+"/"))
      a3map.put(r(2)+'.'+r(4)+'.'+r(5)+'.'+r(6),R(ns+r(0)+"/"))      
    }
    wr = CSVReader("admin4Codes.txt")(CSVReaderSettings.Standard.copy(separator='\t')) // cut allCountries.txt -f 1,8,9,10,11,12,13,14 | grep '\bADM4\b' > admin4Codes.txt
    for (r <- wr) {
      for (code <- r(3).split(",").filter(_!=""))
        a4map.put(code+'.'+r(4)+'.'+r(5)+'.'+r(6)+'.'+r(7),R(ns+r(0)+"/"))
      a4map.put(r(2)+'.'+r(4)+'.'+r(5)+'.'+r(6)+'.'+r(7),R(ns+r(0)+"/"))      
    }
    m.setNsPrefixes(PrefixMapping.Standard)
    m.setNsPrefix("gn",ns)
    m.setNsPrefix("gns",sns)
    m.setNsPrefix("wgs84",WGS84.ns)
    println("1")
    wr = CSVReader("alternateNames.txt")(CSVReaderSettings.Standard.copy(separator='\t',quotechar=null.asInstanceOf[Char]))
    for (r <- wr) {
      val item = R(ns+r(1)+"/")
      if (r(2)=="link") item.addProperty(RDFS.seeAlso,R(r(3)))
      else if (r(2)=="post") item.addProperty(postalCode,r(3))
      else if (r(2)=="fr_1793") item.addProperty(alternateName,r(3),"fr-1793")
      else item.addProperty(alternateName,r(3),r(2))
    }
/*    RDFDataMgr.write(new FileOutputStream("geonames-alternates.nt"), m, RDFFormat.NT)
    m.removeAll()*/
    println("2")
    process(CSVReader("null.txt")(CSVReaderSettings.Standard.copy(separator='\t',quotechar=null.asInstanceOf[Char])))
    println("3")
    process(CSVReader("allCountries.txt")(CSVReaderSettings.Standard.copy(separator='\t',quotechar=null.asInstanceOf[Char])))
    println("4")
    RDFDataMgr.write(new FileOutputStream("geonames.ttl"), m, RDFFormat.TTL)
  }
}
