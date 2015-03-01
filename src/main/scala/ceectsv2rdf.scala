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
import com.bizo.mighty.csv.CSVReaderSettings
import scala.io.Source
import com.hp.hpl.jena.sparql.vocabulary.FOAF

object CEECCSV2RDF extends Anything2RDF {
  
  val sns = "http://ldf.fi/ceec-schema#"
  val ns = "http://ldf.fi/ceec/"

  val Letter = EC("Letter")
  
  val fullText = EDP("fulltext")
  
  val RelationshipCode = EC("Relationship Code")
  val relationshipCode = EOP("relationship")
  val RM = Map("FN"->I(ns+"RelCode_FN",Map("en"->"Nuclear Family"),RelationshipCode),
  "FO" -> I(ns+"RelCode_FO",Map("en"->"Other Family"),RelationshipCode),
  "FS" -> I(ns+"RelCode_FS",Map("en"->"Family Servant"),RelationshipCode),
  "TC" -> I(ns+"RelCode_TC",Map("en"->"Close Friend"),RelationshipCode),
  "C" -> I(ns+"RelCode_C",Map("en"->"C?"),RelationshipCode),
  "T" -> I(ns+"RelCode_C",Map("en"->"T?"),RelationshipCode),
  "RT" -> I(ns+"RelCode_T",Map("en"->"Other"),RelationshipCode))
  
  val SocialMobility = EC("Social Mobility")
  val socialMobility = EOP("social mobility")
  val SM = Map("U"->I(ns+"SocialMobility_U",Map("en"->"Up"),SocialMobility),
      "D" -> I(ns+"SocialMobility_D",Map("en"->"Down"),SocialMobility),
      "N" -> I(ns+"SocialMobility_N",Map("en"->"None"),SocialMobility))

  val Religion = EC("Religion")
  val religion = EOP("religion")
  val RelM = Map("P"->I(ns+"Religion_P",Map("en"->"Protestant"),Religion),
      "A"->I(ns+"Religion_A",Map("en"->"Anglican"),Religion),
      "C"->I(ns+"Religion_C",Map("en"->"Catholic"),Religion),
      "X"->I(ns+"Religion_X",Map("en"->"Unknown"),Religion))
  
  def main(args: Array[String]): Unit = {
    var wr = CSVDictReader("database_person.txt")(CSVReaderSettings.Standard.copy(separator='\t',quotechar='|'))
    for (r <- wr) {
      val p = I(ns+"person_"+encode(r("PersonCode")),r("FirstName")+" "+r("LastName"),CIDOC.Person)
      r.foreach { 
        case (k,"") => 
        case ("PersonCode",v) =>
        case ("SocMob",v) => p.addProperty(socialMobility,SM(v))
        case ("Religion",v) => p.addProperty(religion,RelM(v))
        case (k,v) => p.addProperty(EDP(k),v)
      }
    }
    wr = CSVDictReader("database_letter.txt")(CSVReaderSettings.Standard.copy(separator='\t',quotechar='|'))
    for (r <- wr) {
      val p = I(ns+"letter_"+encode(r("LetterID")),Map("en"->(r("SenderFirstName")+" "+r("SenderLastName")+" to "+r("RecipientFirstName")+" "+r("RecipientLastName")+" on "+r("LetterDate"))),Letter)
      r.foreach { 
        case (k,"") => 
        case ("Sex","M") => p.addProperty(FOAF.gender,SDMXCode.sexMale)
        case ("Sex","F") => p.addProperty(FOAF.gender,SDMXCode.sexFemale)
        case ("LetterID",v) =>
        case ("Sender",v) => p.addProperty(CIDOC.custody_surrendered_by,R(ns+"person_"+encode(v)))
        case ("Recipient",v) => p.addProperty(CIDOC.custody_received_by,R(ns+"person_"+encode(v)))
        case ("RelCode",v) => p.addProperty(relationshipCode,RM(v))
        case (k,v) => p.addProperty(EDP(k),v)
      }
    }
    val sb = new StringBuilder()    
    var clid = ""
    for(line <- Source.fromFile("texts_plain.txt").getLines) {
      if (line.startsWith("<L")) {
        if (clid!="") R(ns+"letter_"+encode(clid)).addProperty(fullText,sb.toString(),"en")
        clid = line.substring(3,line.length-1)
        sb.clear()
      } else {
       sb.append(line)
       sb.append('\n')
      }
    }
    R(ns+"letter_"+encode(clid)).addProperty(fullText,sb.toString(),"en")
    RDFDataMgr.write(new FileOutputStream("ceec.nt"), m, RDFFormat.NT)
    
  }
}
