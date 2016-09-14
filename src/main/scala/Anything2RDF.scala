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
import com.hp.hpl.jena.rdf.model.Property
import org.joda.time.format.ISODateTimeFormat
import com.hp.hpl.jena.datatypes.xsd.XSDDatatype
import com.typesafe.scalalogging.LazyLogging
import com.hp.hpl.jena.graph.NodeFactory

abstract class Anything2RDF extends LazyLogging {

  implicit val m = ModelFactory.createDefaultModel()

  val sns : String
  val ns : String

  private val words = """([a-zA-Z]+)""".r
  private val separators = """([^a-zA-Z]+)""".r

  def makeTimeSpan(name:String,f :(String,String)): Resource = makeTimeSpan(name,Option(f._1),Option(f._2),Option(f._1),Option(f._2))

  def makeTimeSpan(name:String,f:(String,String),t:(String,String)): Resource = makeTimeSpan(name,Option(f._1),Option(f._2),Option(t._1),Option(t._2))

  def makeTimeSpan(name:String,bob:String,eoe:String): Resource = makeTimeSpan(name,Option(bob),Option(eoe),Option(bob),Option(eoe))

  def makeTimeSpan(name:String,bob:Option[String],eob:Option[String],boe:Option[String],eoe:Option[String]): Resource = {
     val ts = I(ns+s"time_${bob.getOrElse("_")}-${eob.getOrElse("_")}-${boe.getOrElse("_")}-${eoe.getOrElse("_")}",name,CIDOC.TimeSpan)
     bob.foreach(ts.addProperty(CIDOC.begin_of_the_begin,_,XSDDatatype.XSDdateTime))
     eob.foreach(ts.addProperty(CIDOC.end_of_the_begin,_,XSDDatatype.XSDdateTime))
     boe.foreach(ts.addProperty(CIDOC.begin_of_the_end,_,XSDDatatype.XSDdateTime))
     eoe.foreach(ts.addProperty(CIDOC.end_of_the_end,_,XSDDatatype.XSDdateTime))
     ts
  }

  def makeDateString(year:String,month:String,date:String): String = {
    var ret = year
    if (!month.isEmpty) {
      ret += '-'
      ret += month
      if (!date.isEmpty) {
        ret += '-'
        ret += date
      }
    }
    ret
  }

  def makeDateTime(year:String,month:String,date:String): (String, String) = {
    val ayear = if (year.startsWith("-")) "-" + "0"*(5-year.length)+year.substring(1); else "0"*(4-year.length)+year
    val bmonth = {
      if (month.isEmpty() || month == "99" || month.matches("^0{1,2}$")) "01"
      else if (month.length()==1) "0"+month
      else month
    }
    val emonth = {
      if (month.isEmpty() || month == "99" || month.matches("^0{1,2}$")) "12"
      else if (month.length()==1) "0"+month
      else month
    }
    val bdate = {
      if (date.isEmpty() || date == "99" || date.matches("^0{1,2}$")) "01"
      else if (date.length()==1) "0"+date
      else date
    }
    val edate = {
      if (date.isEmpty() || date == "99" || date.matches("^0{1,2}$")) ""+ISODateTimeFormat.yearMonth().parseLocalDateTime(year+"-"+emonth).dayOfMonth().withMaximumValue().getDayOfMonth()
      else if (date.length()==1) "0"+date
      else date
    }
    (s"${ayear}-${bmonth}-${bdate}T00:00:00",s"${ayear}-${emonth}-${edate}T23:59:59")
  }

  def camelCase(text: String) : String = separators.replaceAllIn(words.replaceAllIn(text, m => m.matched.toLowerCase.capitalize), "");

  def uncapitalize(toString : String): String =
    if (toString == null) null
    else if (toString.length == 0) ""
    else {
      val chars = toString.toCharArray
      chars(0) = chars(0).toLower
      new String(chars)
    }

  def propertyCamelCase(text: String) : String = separators.replaceAllIn(uncapitalize(words.replaceAllIn(text, m => m.matched.toLowerCase.capitalize)), "");

  def P(s: String) = m.createProperty(s)

  def P(uri: String, labels : Map[String,String], c : Resource): Property = {
    val r = m.createProperty(uri)
    labels.foreach{ case (lang: String, label : String) => r.addProperty(SKOS.prefLabel,label,lang) }
    r.addProperty(RDF.`type`,c)
    r
  }

  def ANE(r: Resource, p: Property, o : String) : Unit = {
    if (!o.trim.isEmpty()) r.addProperty(p,o)
  }

  def ANE(r: Resource, p: Property, o : String, l : String) : Unit = {
    if (!o.trim.isEmpty()) r.addProperty(p,o,l)
  }

  def EOP(s: String) = P(sns+propertyCamelCase(s),Map("en"->s),OWL.ObjectProperty)

  def EDP(s: String) = P(sns+propertyCamelCase(s),Map("en"->s),OWL.DatatypeProperty)

  def R(s: String) = m.createResource(s)

  def RN(s: String) = NodeFactory.createURI(s)

  def LN(s: String) = NodeFactory.createLiteral(s)

  def LN(s: String, lang: String) = NodeFactory.createLiteral(s, lang)

  def BN() = NodeFactory.createAnon()

  def EC(s: String) = I(sns+camelCase(s),Map("en"->s),OWL.Class)


  def I(uri: String, labels : Map[String,String], c : Resource): Resource = {
    val r = m.createResource(uri)
    labels.foreach{ case (lang: String, label : String) => r.addProperty(SKOS.prefLabel,label,lang) }
    r.addProperty(RDF.`type`,c)
    r
  }

  def I(uri: String, c : Resource): Resource = {
    val r = m.createResource(uri)
    r.addProperty(RDF.`type`,c)
    r
  }

  def I(uri: String, label : String, c : Resource): Resource = {
    val r = m.createResource(uri)
    r.addProperty(SKOS.prefLabel,label)
    r.addProperty(RDF.`type`,c)
    r
  }

  def encode(s : String): String = URLEncoder.encode(s,"UTF-8")

}
