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
import java.io.FileInputStream
import com.hp.hpl.jena.datatypes.xsd.XSDDatatype
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._
import scala.collection.mutable.HashSet
import com.hp.hpl.jena.rdf.model.Statement
import scala.collection.mutable.ArrayBuffer

object PNR2RDF extends Anything2RDF {

  val sns = "http://ldf.fi/pnr/schema#"
  val ns = "http://ldf.fi/pnr/"
  
  val PlaceType = I(sns+"PlaceType",Map("fi"->"Paikkatyyppi","en"->"Place Type"),OWL.Class)
  val Language = I(sns+"Language",Map("fi"->"Kieli","en"->"Language"),OWL.Class)
  //val Municipality = I(sns+"Municipality",Map("fi"->"Kunta","en"->"Municipality"),OWL.Class)
  //val Province = I(sns+"Province",Map("fi"->"Lääni","en"->"Province"),OWL.Class)
  //val Region = I(sns+"Region",Map("fi"->"Maakunta","en"->"Region"),OWL.Class)
  val SubRegion = I(sns+"SubRegion",Map("fi"->"Seutukunta","en"->"Sub-region"),OWL.Class)
  val LargeArea = I(sns+"LargeArea",Map("fi"->"Suuralue","en"->"Large Area"),OWL.Class)
  val MapScale = I(sns+"MapScale",Map("fi"->"Mittakaava","en"->"Map Scale"),OWL.Class)

  def open(file: String) : XMLEventReader = {
    val t = new FileInputStream(file)
    t.read
    t.read
    t.read
    new XMLEventReader(Source.fromInputStream(t))
  }

  val lmap = Map("fin"->"fi","swe"->"sv","eng"->"en","sme"->"se","smn"->"smn","sms"->"sms")

  def process(file:String,prefix:String, clazz:Resource,ns:String=sns) {
    var xml = open(file)
    var item : Resource = null
    while (xml.hasNext) xml.next match {
      case EvElemStart(_,"enumeration", attrs, _) => 
        item=R(ns+prefix+"_"+attrs("value")(0).text)
        item.addProperty(RDF.`type`,clazz)
      case EvElemStart(_,"documentation", attrs, _) => 
        item.addProperty(SKOS.prefLabel,xml.next.asInstanceOf[EvText].text.trim,lmap(attrs.value(0).text))
      case _ =>
    }
  }
  
  val WGS84_MAJOR = 6378137.0
  val WGS84_MINOR = 6356752.314245
  val FLATTENING = 1.0 / 298.257223563
  val SCALE_FACTOR = 0.9996
  val FALSE_EASTING = 500000.0
  val FALSE_NORTHING = 0.0
  val CENTRAL_MERIDIAN = 27.0
  val central_meridian = CENTRAL_MERIDIAN * (Math.PI / 180)
  
  def to_wgs84(north_coordinate : Double, east_coordinate : Double) : (Double,Double) = {
    val n = FLATTENING / (2.0 - FLATTENING)
    val a1 = WGS84_MAJOR / (1.0 + n) * (1.0 + Math.pow(n,2.0) / 4.0 + Math.pow(n,4.0) / 64.0)
    val e = Math.sqrt(2.0 * FLATTENING - Math.pow(FLATTENING,2.0))
    val h1 = 1.0 / 2.0 * n - 2.0 / 3.0 * Math.pow(n,2.0) + 37.0/96.0 * Math.pow(n,3.0) - 1.0/360.0 * Math.pow(n,4.0)
    val h2 = 1.0 / 48.0 * Math.pow(n,2.0) + 1.0/15.0 * Math.pow(n,3.0) - 437.0/1440.0 * Math.pow(n,4.0)
    val h3 = 17.0 / 480.0 * Math.pow(n,3.0) - 37.0 / 840.0 * Math.pow(n,4.0)
    val h4 = 4397.0 / 161280.0 * Math.pow(n,4.0)

    val ee = north_coordinate / (a1 * SCALE_FACTOR)
    val nn = (east_coordinate - FALSE_EASTING) / (a1 * SCALE_FACTOR)

    val e1p = h1 * Math.sin(2.0 * ee) * Math.cosh(2.0 * nn)
    val e2p = h2 * Math.sin(4.0 * ee) * Math.cosh(4.0 * nn)
    val e3p = h3 * Math.sin(6.0 * ee) * Math.cosh(6.0 * nn)
    val e4p = h4 * Math.sin(8.0 * ee) * Math.cosh(8.0 * nn)

    val nn1p = h1 * Math.cos(2.0 * ee) * Math.sinh(2.0 * nn)
    val nn2p = h2 * Math.cos(4.0 * ee) * Math.sinh(4.0 * nn)
    val nn3p = h3 * Math.cos(6.0 * ee) * Math.sinh(6.0 * nn)
    val nn4p = h4 * Math.cos(8.0 * ee) * Math.sinh(8.0 * nn)

    val ep = ee - e1p - e2p - e3p - e4p

    val nnp = nn - nn1p - nn2p - nn3p - nn4p
    val be = Math.asin(Math.sin(ep) / Math.cosh(nnp))

    val q = breeze.numerics.asinh(Math.tan(be))
    var qp = q + e * breeze.numerics.atanh(e * Math.tanh(q))
    qp = q + e * breeze.numerics.atanh(e * Math.tanh(qp))
    qp = q + e * breeze.numerics.atanh(e * Math.tanh(qp))
    qp = q + e * breeze.numerics.atanh(e * Math.tanh(qp))

    val lat = (Math.atan(Math.sinh(qp)) * 180.0) / Math.PI
    val lon = ((central_meridian + Math.asin(Math.tanh(nnp) / Math.cos(be))) * 180.0) / Math.PI
    (lat, lon)
  }
  
  def main(args: Array[String]): Unit = {
    process("kieli.xsd","language",Language)
    //process("laani.xsd","province",Province)
    //process("kunta.xsd","municipality",Municipality)
    process("paikkatyyppiryhma.xsd","place_type",PlaceType)
    process("paikkatyyppialaryhma.xsd","place_type",PlaceType)
    process("paikkatyyppi.xsd","place_type",PlaceType)
    process("mittakaavarelevanssi.xsd","map_scale",MapScale)
    //process("maakunta.xsd","region",Region)
    val m2 = ModelFactory.createDefaultModel()
    m2.add(m)
    m.removeAll()
    process("seutukunta.xsd","subregion",SubRegion,ns)
    process("suuralue.xsd","large_area",LargeArea,ns)
    val xml = new XMLEventReader(Source.fromFile("paikka.xml"))
    var item : Resource = null
    var name = ""
    var lang = ""
    var kunta : Resource = null
    var seutukunta : Resource = null
    var maakunta : Resource = null
    var laani : Resource = null
    var paikkatyyppi : Resource = null
    var paikkatyyppiRyhma : Resource = null
    var paikkatyyppiAlaRyhma : Resource = null
    var replaceMap = new HashMap[Resource,Resource]()
    var ptt = ""
    while (xml.hasNext) xml.next match {
      case EvElemStart(_,"Paikka", attrs, _) => 
        item=R(ns+attrs.value(0).text)
      case EvElemStart(_,"pos",_, _) =>
        val opos = xml.next.asInstanceOf[EvText].text
        val parts = opos.split(" ")
        val (lat,lon) = to_wgs84(parts(1).toDouble,parts(0).toDouble)
        item.addProperty(WGS84.lat,""+lat,XSDDatatype.XSDdecimal)
        item.addProperty(WGS84.long,""+lon,XSDDatatype.XSDdecimal)
      case EvElemStart(_,"paikkatyyppiKoodi", _, _) =>
        ptt = xml.next.asInstanceOf[EvText].text
        paikkatyyppi = R(sns+"place_type"+"_"+ptt)
        item.addProperty(RDF.`type`,paikkatyyppi)
      case EvElemStart(_,"paikkatyyppiryhmaKoodi", _, _) =>
        paikkatyyppiRyhma = R(sns+"place_type"+"_"+xml.next.asInstanceOf[EvText].text)
      case EvElemStart(_,"paikkatyyppialaryhmaKoodi", _, _) =>
        paikkatyyppiAlaRyhma = R(sns+"place_type"+"_"+xml.next.asInstanceOf[EvText].text)
        m2.add(paikkatyyppi,RDFS.subClassOf,paikkatyyppiAlaRyhma)
        m2.add(paikkatyyppiAlaRyhma,RDFS.subClassOf,paikkatyyppiRyhma)
      case EvElemStart(_,"tm35Fin7Koodi", _, _) =>
        item.addProperty(P(sns+"tm35Fin7Koodi"),xml.next.asInstanceOf[EvText].text)
      case EvElemStart(_,"ylj7Koodi", _, _) =>
        item.addProperty(P(sns+"ylj7Koodi"),xml.next.asInstanceOf[EvText].text)
      case EvElemStart(_,"pp6Koodi", _, _) =>
        item.addProperty(P(sns+"pp6Koodi"),xml.next.asInstanceOf[EvText].text)
      case EvElemStart(_,"kuntaKoodi", _, _) =>
        kunta = R(ns+"municipality_"+xml.next.asInstanceOf[EvText].text)
        ptt match {
          case "540" | "550" => replaceMap.put(kunta,item)
          case "580" | "575" =>
          case _ => 
            item.addProperty(CIDOC.place_falls_within,kunta)
        }
      case EvElemStart(_,"seutukuntaKoodi", _, _) =>
        seutukunta = R(ns+"subregion_"+xml.next.asInstanceOf[EvText].text)
        kunta.addProperty(CIDOC.place_falls_within,seutukunta)
      case EvElemStart(_,"maakuntaKoodi", _, _) =>
        maakunta = R(ns+"region_"+xml.next.asInstanceOf[EvText].text)
        ptt match {
          case "575" => replaceMap.put(maakunta,item)
          case _ => 
        }
        seutukunta.addProperty(CIDOC.place_falls_within,maakunta)
      case EvElemStart(_,"laaniKoodi", _, _) =>
        val laani = R(ns+"province_"+xml.next.asInstanceOf[EvText].text)
        ptt match {
          case "580" => replaceMap.put(laani,item)
          case _ => 
        }
        kunta.addProperty(CIDOC.place_falls_within,laani)
      case EvElemStart(_,"suuralueKoodi", _, _) =>
        maakunta.addProperty(CIDOC.place_falls_within,R(ns+"large_area_"+xml.next.asInstanceOf[EvText].text))
      case EvElemStart(_,"mittakaavarelevanssiKoodi", _, _) =>
        item.addProperty(P(sns+"mapScale"),R(sns+"map_scale_"+xml.next.asInstanceOf[EvText].text))
      case EvElemStart(_,"kirjoitusasu", _, _) =>
        name = xml.next.asInstanceOf[EvText].text
      case EvElemStart(_,"kieliKoodi", _, _) =>
        lang = xml.next.asInstanceOf[EvText].text
        item.addProperty(SKOS.prefLabel,name,lmap(lang))
      case EvElemStart(_,"kieliVirallisuusKoodi", _, _) =>
        if (xml.next.asInstanceOf[EvText].text=="1") item.addProperty(P(sns+"officialLanguage"),R(sns+"language_"+lang))
      case EvElemStart(_,"kieliEnemmistoKoodi", _, _) =>
        if (xml.next.asInstanceOf[EvText].text=="1") item.addProperty(P(sns+"majorityLanguage"),R(sns+"language_"+lang))
        
/*<pnr:paikkaLuontiAika>2008-12-06T00:00:00.000</pnr:paikkaLuontiAika>
<pnr:paikkaMuutosAika>2008-12-06T00:00:00.000</pnr:paikkaMuutosAika>*/
      case _ =>
    }
    m2.setNsPrefix("pnrs", sns)
    m2.setNsPrefix("pnr",ns)
    m2.setNsPrefix("crm",CIDOC.ns)
    m2.setNsPrefix("skos",SKOS.ns)
    m2.setNsPrefix("rdfs",RDFS.getURI)
    RDFDataMgr.write(new FileOutputStream("pnr-schema.ttl"), m2, RDFFormat.TTL)
    for ((k1,k2) <- replaceMap) {
      val toDel = new ArrayBuffer[Statement]
      val toAdd = new ArrayBuffer[Statement]
      for (s<-k1.listProperties()) {
        toDel += s
        toAdd += m.createStatement(k2,s.getPredicate,s.getObject)
      }
      for (s<-m.listStatements(null,null,k1)) {
        toDel += s
        toAdd += m.createStatement(s.getSubject,s.getPredicate,k2)
      }
      for (s <- toDel) m.remove(s)
      for (s <- toAdd) m.add(s)
    }
    RDFDataMgr.write(new FileOutputStream("pnr.nt"), m, RDFFormat.NT)
    System.exit(0)
  }
}
