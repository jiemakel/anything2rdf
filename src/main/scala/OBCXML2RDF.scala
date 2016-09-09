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
import com.bizo.mighty.csv.CSVDictReader
import com.bizo.mighty.csv.CSVReaderSettings
import scala.io.Source
import org.apache.jena.sparql.vocabulary.FOAF
import scala.xml.pull.XMLEventReader
import scala.xml.pull.EvElemStart
import scala.xml.pull.EvText
import scala.xml.pull.EvElemEnd
import scala.xml.MetaData

object OBCXML2RDF extends Anything2RDF {
  
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

  val Authenticity = EC("Authenticity")
      
  val authenticity = EOP("authenticity")
  val uncertainAuthenticity = EOP("uncertain authenticity")
  val authenticityM = Map('A'->I(ns+"Authenticity_A",Map("en"->"Holograph"),Authenticity),
      'B'->I(ns+"Authenticity_B",Map("en"->"Holograph; writer’s social background partly unknown"),Authenticity),
      'C'->I(ns+"Authenticity_C",Map("en"->"Later Copy"),Authenticity),
      'D'->I(ns+"Authenticity_D",Map("en"->"Uncertain authenticity; copy & writer’s social bg partly unknown"),Authenticity),
      'E'->I(ns+"Authenticity_E",Map("en"->"Modernized"),Authenticity),
      'S'->I(ns+"Authenticity_S",Map("en"->"Scribal/Secretarial"),Authenticity))
      
  val Religion = EC("Religion")
  val religion = EOP("religion")
  val RelM = Map("P"->I(ns+"Religion_P",Map("en"->"Protestant"),Religion),
      "A"->I(ns+"Religion_A",Map("en"->"Anglican"),Religion),
      "C"->I(ns+"Religion_C",Map("en"->"Catholic"),Religion),
      "X"->I(ns+"Religion_X",Map("en"->"Unknown"),Religion))
      
  val Education = EC("Education")
  val education = EOP("education")
  val educationDetails = EDP("education details")
  val uncertainEducation = EOP("uncertain education")
  
  val Higher = I(ns+"Education_H",Map("en"->"Higher"),Education)
  val Cambridge = I(ns+"Education_C",Map("en"->"Higher (Cambridge)"),Education)
  Cambridge.addProperty(RDFS.subClassOf, Higher)
  val Oxford = I(ns+"Education_O",Map("en"->"Higher (Oxford)"),Education)
  Oxford.addProperty(RDFS.subClassOf, Higher)
  val Foreign = I(ns+"Education_F",Map("en"->"Higher (Foreign)"),Education)
  Foreign.addProperty(RDFS.subClassOf, Higher)
  val Inns = I(ns+"Education_I",Map("en"->"Higher (Inns of Court)"),Education)
  Inns.addProperty(RDFS.subClassOf, Higher)
  
  val EduM = Map("A"->I(ns+"Education_A",Map("en"->"Apprenticed"),Education),
      "A"->I(ns+"Education_A",Map("en"->"Apprenticed"),Education),
      "E"->I(ns+"Education_E",Map("en"->"Elementary"),Education),
      "H"->Higher,
      "C"->Cambridge,
      "I"->Inns,
      "O"->Oxford,
      "PC"->I(ns+"Education_PC",Map("en"->"Private/Self: Classical"),Education),
      "PN"->I(ns+"Education_PN",Map("en"->"Private/Self: Non-Classical"),Education),
      "S"->I(ns+"Education_S",Map("en"->"Secondary"),Education),
      "F"->Foreign)
  /*, E (elementary), H (higher), HC (higher: cambridge),
HI (higher: inns of court), HO (higher: oxford), PC (private/self: classical), PN
(private/self: non-classical), S (secondary), HF (higher: foreign),")*/
      
  val Region = EC("Region")
  val pbirth = EOP("place of birth")
  val region = EOP("region")
  val RegionM = Map("N"->I(ns+"Region_N",Map("en"->"North"),Region),
      "F"->I(ns+"Region_F",Map("en"->"East Anglia"),Region),
      "H"->I(ns+"Region_H",Map("en"->"Home Counties"),Region),
      "L"->I(ns+"Region_L",Map("en"->"London"),Region),
      "C"->I(ns+"Region_C",Map("en"->"Court"),Region),
      "O"->I(ns+"Region_O",Map("en"->"Other"),Region),
      "A"->I(ns+"Region_A",Map("en"->"Abroad"),Region))
  
  val Role = EC("Role")
  val roleP = EOP("role")
  val Rank = EC("Rank")
  val uncertainRank = EOP("uncertain rank")
  val rank = EOP("rank")
  val senderRank = EOP("sender rank")
  val uncertainSenderRank = EOP("uncertain sender rank")
  val recRank = EOP("receiver rank")
  val uncertainRecRank = EOP("uncertain receiver rank")
  val fatherRank = EOP("father's rank")
  val uncertainFatherRank = EOP("uncertain father's rank")
  val RankM = Map("R"->I(ns+"Rank_R",Map("en"->"Royalty"),Rank),
      "N"->I(ns+"Rank_N",Map("en"->"Nobility"),Rank),
      "GU"->I(ns+"Rank_GU",Map("en"->"Upper Gentry"),Rank),
      "GL"->I(ns+"Rank_GL",Map("en"->"Lower Gentry"),Rank),
      "G"->I(ns+"Rank_G",Map("en"->"Gentry"),Rank),
      "P"->I(ns+"Rank_P",Map("en"->"Professional"),Rank),
      "CU"->I(ns+"Rank_CU",Map("en"->"Upper Clergy"),Rank),
      "CL"->I(ns+"Rank_CL",Map("en"->"Lower Clergy"),Rank),
      "M"->I(ns+"Rank_M",Map("en"->"Merchant"),Rank),
      "O"->I(ns+"Rank_O",Map("en"->"Other"),Rank))
      
  val Migration = EC("Migration")
  val migration = EOP("migration")
  val migrationDetails = EDP("migration details")
  val MigM = Map('Y'->I(ns+"Migration_Y",Map("en"->"Yes"),Migration),
      'L'->I(ns+"Migration_L",Map("en"->"London"),Migration),
      'A'->I(ns+"Migration_A",Map("en"->"Abroad"),Migration))
      
  val Occupation = EC("Occupation")
  val occupationP = EOP("occupation")
  val occupationDetailsP = EDP("occupation details")
      /*
      Sex: F (female), M (male)
2. Region / PBirth: N (north), F (east anglia), H (home counties), L (london), C (court),
O (other), A (abroad)
3. County: BDF, BKM, BRK, CAM, CHS, CON, CRT, CUL, DBY, DEV, DOR, DUR,
ESS, GLS, HAM, HEF, HRT, HUN, KEN, LAN, LEI, LIN, LND, MDX, NBL,
NFK, NTH, NTT, OXF, RUT, SAL, SFK, SOM, SRY, SSX, STS, WAR, WES,
WIL, WOR, YKS, ERY, WRY, NRY, CHI, IOM, IOW, ABR (+A, C, F, H, L,N, O)
5. Rank / FatherRank: R (royalty), N (nobility), GU (gentry upper), GL (gentry lower),
G (gentry), P (professional), CU (clergy upper), CL (clergy lower), M (merchant),
O (other), (+?)
6. MigCode: Y (yes), YL (yes: london), YA (yes: abroad), YLA (yes: london & abroad)
7. EduCode: A (apprenticed), E (elementary), H (higher), HC (higher: cambridge),
HI (higher: inns of court), HO (higher: oxford), PC (private/self: classical), PN
(private/self: non-classical), S (secondary), HF (higher: foreign), (+ C, O, I, F) (+?)
8. Religion: P (protestant), A (anglican), C (catholic), X (unknown)
9. SentLettcont / RecLettcont: M (mixed), B (business), P (private), N (news), O (of-
ficial), W (other), L (love), D (duty), T (travel), F (family) - mikä tahansa yhdistelmä
näistä (kentän sisältöä ei tarkisteta)*/
  
  val yearP = EDP("year")
  val wordcountP = EDP("wordcount")
      
  def get(key: String)(implicit attrs: MetaData): Option[String] = {
    if (attrs(key)!=null && attrs(key)(0).text!="") Some(attrs(key)(0).text.trim)
    else None
  }
      
  def main(args: Array[String]): Unit = {
    var speechId = 1
    for (file <- new java.io.File("obc_clear/").listFiles) {
      println("Processing: "+file)
      val xml = new XMLEventReader(Source.fromFile(file,"ISO-8859-1"))
      var header = ""
      while (xml.hasNext) xml.next match {
        case EvElemStart(_,"header", _, _) => header=xml.next.asInstanceOf[EvText].text
        case EvElemStart(_,"speech", attrs, _) =>
          implicit val iattrs = attrs
          if (attrs("obc-spi")!=null && attrs("obc-yea")!=null) { 
            val speakerId = attrs("obc-spi")(0).text.trim
            val speakerName = get("obc-prn").orElse(get("obc-pbl"))
            val year = attrs("obc-yea")(0).text.trim
            val gender = get("obc-sex")
            val role = get("obc-rol")
            val occupation1Id = get("obc-hc1")
            val occupation1Label = get("obc-hl1")
            val occupation1Detail = get("obc-oc1")
            val occupation2Id = get("obc-hc2")
            val occupation2Label = get("obc-hl2")
            val occupation2Detail = get("obc-oc2")
            val speech = I(ns+"speech_"+speechId,Map("en"->(speakerName.getOrElse("?")+ " in "+header)), Letter)
            val speaker = I(ns+"speaker_"+speakerId,speakerName.getOrElse("?"),CIDOC.Person)
            speech.addProperty(yearP,year)
            speech.addProperty(CIDOC.custody_surrendered_by,speaker)
            gender.foreach { g => speaker.addProperty(FOAF.gender,if (g == "f") SDMXCode.sexFemale else SDMXCode.sexMale) }
            role.foreach { r => speech.addProperty(roleP,I(ns+"role_"+encode(r),Map("en"->r), Role)) }
            occupation1Id.foreach { r => speaker.addProperty(occupationP, I(ns+"occupation_"+encode(r),Map("en"->occupation1Label.getOrElse(occupation1Detail.get)), Occupation)) }
            occupation1Detail.foreach { r => speaker.addProperty(occupationDetailsP, r) }
            occupation2Id.foreach { r => speaker.addProperty(occupationP, I(ns+"occupation_"+encode(r),Map("en"->occupation2Label.getOrElse(occupation2Detail.get)), Occupation)) }
            occupation2Detail.foreach { r => speaker.addProperty(occupationDetailsP, r) }
            speechId+=1
            var t: String = ""
            var break: Boolean = false
            while (!break && xml.hasNext) xml.next match {
              case EvText(text) => t+=text
              case EvElemEnd(_,"speech") => break=true
              case _ => 
            }
            speech.addProperty(fullText,t)
            speech.addProperty(wordcountP,""+ ("\\s+".r.findAllIn(t).length))
          }
        case _ => 
      }
    }
    RDFDataMgr.write(new FileOutputStream("obc.nt"), m, RDFFormat.NT)
    
  }
}
