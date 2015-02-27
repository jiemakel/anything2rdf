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
import com.bizo.mighty.csv.CSVDictReader

object ANDCSV2RDF extends Anything2RDF {
  
  val sns = "http://ldf.fi/and-schema#"
  val ns = "http://ldf.fi/and/"

  val period = EOP("period")
  val authorial_presence = EDP("authorial presence")
  val mentions = EOP("mentions")
  val genre = EOP("genre")
  val practitioner_type = EOP("practitioner type")
  val prefatory_names = EOP("prefatory names")
  val Work = EC("Work")
  val Person = EC("Person")
  val Genre = EC("Genre")
  val Period = EC("Period")
  val Date = EC("Date")
  val PractitionerType = EC("Practitioner Type")
  val prose = I(sns + "practitionerTypeProse", Map("en" -> "Prose"),PractitionerType)
  val canonical = I(sns + "practitionerTypeCanonical", Map("en" -> "Canonical"),PractitionerType)
  val noncanonical = I(sns + "practitionerTypeNonCanonical", Map("en" -> "Non-Canonical"),PractitionerType)
  val poetry = I(sns + "PractitionerTypePoetry", Map("en" -> "Poetry"),PractitionerType)
  val roman = I(sns + "PractitionerTypeRoman", Map("en" -> "Roman"),PractitionerType)
  val contemporary = I(sns + "PractitionerTypeContemporary", Map("en" -> "Contemporary"),PractitionerType)
  val myth = I(sns + "PractitionerTypeMyth", Map("en" -> "Myth"),PractitionerType)
  val oral = I(sns + "PractitionerTypeOral", Map("en" -> "Oral"),PractitionerType)

  def pmentions(s : String, w : Resource, ptypes : Resource*) = s.trim.split(',').map(s => s.trim).filter(s => !s.isEmpty).foreach(s => {
        val p2 = I(ns+"person_"+encode(s),s,Person)
        w.addProperty(mentions,p2)
        ptypes.foreach(ptype => p2.addProperty(practitioner_type,ptype))
      })
  
  def main(args: Array[String]): Unit = {
    val wr = CSVDictReader("works.csv")
    for (r <- wr) {
      val w = I(ns+"work_"+encode(r("Work_id")),Map("en"->r("Work")),Work)
      val a = I(ns+"person_"+encode(r("Author")),r("Author"),Person)
      val d = I(ns+"date_"+encode(r("Date")),r("Date"),Date)
      val p = I(ns+"period_"+encode(r("Period")),Map("en"->r("Period")),Period)
      val g = I(ns+"genre_"+encode(r("Genre").toLowerCase()),Map("en"->r("Genre").toLowerCase()),Genre)
      w.addProperty(DC_11.creator,a)
      w.addProperty(DC_11.date,d)
      w.addProperty(genre,g)
      w.addProperty(authorial_presence,r("Authorial presence"),"en")
      w.addProperty(prefatory_names,r("Prefatory names?"),"en")
      w.addProperty(period,p)
      pmentions(r("Canonical prose practitioners"),w,prose,canonical)
      pmentions(r("Canonical poetry practitioners"),w,poetry,canonical)
      pmentions(r("Non-canonical prose practitioners"),w,prose,noncanonical)
      pmentions(r("Non-canonical poetry practitioners"),w,poetry,noncanonical)
      pmentions(r("Roman prose practitioners"),w,prose,roman)
      pmentions(r("contemporary practitioners"),w,contemporary)
      pmentions(r("myth practitioners"),w,myth)
    }
    val nr = CSVDictReader("names.csv")
    for (r <- nr) {
      val a = I(ns+"person_"+encode(r("Name")),r("Name"),Person)
      val p = I(ns+"period_"+encode(r("Name_period")),Map("en"->r("Name_period")),Period)
      a.addProperty(period,p)
      if (!r("Name_notes").isEmpty)
        a.addProperty(DC_11.description,r("Name_notes"),"en")
      r("Name_type").trim.split(',').map(s => s.trim).filter(s => !s.isEmpty).foreach {
        case "oral" => a.addProperty(practitioner_type,oral)
        case "prose" => a.addProperty(practitioner_type, prose)
        case "poetry" => a.addProperty(practitioner_type, poetry)
        case "myth" => a.addProperty(practitioner_type, myth)
        case "contemp" => a.addProperty(practitioner_type, contemporary)
        case unknown => println("Unknown ptype " + unknown)
      }
    }
    RDFDataMgr.write(new FileOutputStream("/tmp/marc2bs-out.nt"), m, RDFFormat.NT)
  }
}
