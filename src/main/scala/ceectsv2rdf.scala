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
  
  def main(args: Array[String]): Unit = {
    var wr = CSVDictReader("database_person.txt")(CSVReaderSettings.Standard.copy(separator='\t',quotechar='|'))
    for (r <- wr) {
      val p = I(ns+"person_"+encode(r("PersonCode")),r("FirstName")+" "+r("LastName"),CIDOC.Person)
      r.foreach { 
        case (k,"") => 
        case ("PersonCode",v) =>
        case ("SocMob",v) => p.addProperty(socialMobility,SM(v))
        case ("Religion",v) => p.addProperty(religion,RelM(v))
        case ("EduCode",v) => {
          val (v2,prop) = if (v.endsWith("?")) (v.substring(0,v.length-1),uncertainEducation) else (v,education)
          if (v2.startsWith("H") && v2.length>1) v2.substring(1).foreach(c => p.addProperty(prop,EduM(""+c)))
          else p.addProperty(prop,EduM(v2)) 
        }
        case ("MigCode",v) => v.foreach(v => p.addProperty(migration,MigM(v)))
        case ("Region",v) => p.addProperty(region,RegionM(v))
        case ("PBirth",v) => p.addProperty(pbirth,RegionM(v))
        case ("Sex","M") => p.addProperty(FOAF.gender,SDMXCode.sexMale)
        case ("Sex","F") => p.addProperty(FOAF.gender,SDMXCode.sexFemale)
        case ("Rank",v) => if (v.endsWith("?")) p.addProperty(uncertainRank,RankM(v.substring(0,v.length-1)))
        else p.addProperty(rank,RankM(v))
        case ("FatherRank",v) => if (v.endsWith("?")) p.addProperty(uncertainFatherRank,RankM(v.substring(0,v.length-1)))
        else p.addProperty(fatherRank,RankM(v))
        case (prop,"?") =>
        case ("Education",v) => p.addProperty(educationDetails,v)
        case ("Migration",v) => p.addProperty(migrationDetails,v)
        case (prop,"Y") => p.addLiteral(EDP(prop),true)
        case (prop,"N") => p.addLiteral(EDP(prop),false)
        case (k,v) => p.addProperty(EDP(k.charAt(0).toLower+k.substring(1)),v)
      }
    }
    wr = CSVDictReader("database_letter.txt")(CSVReaderSettings.Standard.copy(separator='\t',quotechar='|'))
    for (r <- wr) {
      var label = r("SenderFirstName")+" "+r("SenderLastName")+" to "+r("RecipientFirstName")+" "+r("RecipientLastName")
      if (!r("LetterDate").isEmpty()) {
        label += " on "+r("LetterDate")+", "
      } else label +=" in "
      label += r("Year")
      val p = I(ns+"letter_"+encode(r("LetterID")),Map("en"->(label)),Letter)
      r.foreach { 
        case (k,"") => 
        case ("Authenticity",v) => 
          if (v.endsWith("?")) v.substring(0,v.length-1).foreach(c => p.addProperty(uncertainAuthenticity,authenticityM(c))) 
          else v.foreach(c => p.addProperty(authenticity,authenticityM(c)))
        case ("LetterID",v) =>
        case ("Place",v) => v.split(",").map(_.trim.replace("\\W*$","")).foldRight(None:Option[Resource]) { (lv,sp) => 
          val cp = I(ns+"place_"+encode(lv),lv,CIDOC.Place)
          sp.foreach { cp2 => cp.addProperty(CIDOC.place_falls_within,cp2) }
          Some(cp) }.foreach{place => 
            place.addProperty(SKOS.altLabel,v)
            p.addProperty(CIDOC.took_place_at,place)
          }
        case ("Sender",v) => p.addProperty(CIDOC.custody_surrendered_by,R(ns+"person_"+encode(v)))
        case ("Recipient",v) => p.addProperty(CIDOC.custody_received_by,R(ns+"person_"+encode(v)))
        case ("RelCode",v) => p.addProperty(relationshipCode,RM(v))
        case ("RecRank",v) => if (v.endsWith("?")) p.addProperty(uncertainRecRank,RankM(v.substring(0,v.length-1)))
        else p.addProperty(recRank,RankM(v))
        case ("SenderRank",v) => if (v.endsWith("?")) p.addProperty(uncertainSenderRank,RankM(v.substring(0,v.length-1)))
        else p.addProperty(senderRank,RankM(v))
        case (prop,"Y") => p.addLiteral(EDP(prop),true)
        case (prop,"N") => p.addLiteral(EDP(prop),false)
        case (prop,"?") =>
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
