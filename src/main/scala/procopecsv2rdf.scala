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
import com.hp.hpl.jena.sparql.vocabulary.FOAF
import com.hp.hpl.jena.vocabulary.DC
import com.hp.hpl.jena.vocabulary.DC_11
import com.hp.hpl.jena.vocabulary.RDFS
import com.hp.hpl.jena.datatypes.xsd.XSDDatatype
import com.hp.hpl.jena.shared.PrefixMapping
import org.joda.time.format.ISODateTimeFormat
import com.bizo.mighty.csv.CSVDictReader

object PROCOPECSV2RDF extends Anything2RDF {

  val sns = "http://ldf.fi/procope-schema#"
  val ns = "http://ldf.fi/procope/"

  val Source = EC("Source")
  val sourceP = EOP("source")
  val Network = EC("Network")
  val EgoNetwork = EC("Ego Network")
  EgoNetwork.addProperty(RDFS.subClassOf,Network)
  val egonetworkP = EOP("ego network")
  val eeIDP = EDP("Electronic Enlightenment ID")
  val Occupation = EC("Occupation")
  val occupationP = EOP("occupation")
  val PoliticalRank = EC("Political Rank")
  val politicalRankP = EOP("political rank")
  val MilitaryRank = EC("Military Rank")
  val militaryRankP = EOP("military rank")
  val recipientLetterCountP = EDP("recipient letter count")
  val authorLetterCountP = EDP("author letter count")
  val totalLetterCountP = EDP("total letter count")
  val cnrsdIDP = EDP("CNRS-D ID")
  val reversedNameP = EDP("full name reversed")
  val particuleP = EDP("particule")
  val Relationship = EC("Relationship")
  val relationshipP = EOP("relationships")
  val Title = EC("Title")
  val titleP = EOP("title")
  val AristocraticTitle = EC("Aristocratic Title")
  AristocraticTitle.addProperty(RDFS.subClassOf,Title)
  val ClericalTitle = EC("Clerical Title")
  ClericalTitle.addProperty(RDFS.subClassOf,Title)
  val wikipediaURLP = EDP("Wikipedia URL")
  val viafIDP = EDP("VIAF ID")
  val KnowledgeNetwork = EC("Knowledge Network")
  KnowledgeNetwork.addProperty(RDFS.subClassOf,Network)
  val knowledgeNetworkP = EOP("knowledge network")
  val SocialNetwork = EC("Social Network")
  val socialNetworkP = EOP("social network")
  SocialNetwork.addProperty(RDFS.subClassOf,Network)
  val ProfessionalNetwork = EC("Professional Network")
  val professionalNetworkP = EOP("professional network")
  ProfessionalNetwork.addProperty(RDFS.subClassOf,Network)
  val ReligiousNetwork = EC("Religious Network")
  val religiousNetworkP = EOP("religious network")
  ReligiousNetwork.addProperty(RDFS.subClassOf,Network)
  val Nationality = EC("Nationality")
  val nationalityP = EOP("nationality")
  val possibleNationalityP = EOP("possible nationality")
  val Salon = EC("Salon")
  val salonP = EOP("salon")
  val Academy = EC("Academy")
  val academyP = EOP("academy")

  val Building = EC("Building").addProperty(RDFS.subClassOf, CIDOC.Place)

  val Park = EC("Park").addProperty(RDFS.subClassOf, CIDOC.Place)

  val City = EC("City").addProperty(RDFS.subClassOf, CIDOC.Place)
  
  val Country = EC("Country").addProperty(RDFS.subClassOf, CIDOC.Place)
  
  val Province = EC("Province").addProperty(RDFS.subClassOf, CIDOC.Place)
  
  val bdateP = EOP("birth date")
  val ddateP = EOP("death date")
  val possibleBDateP = EOP("possible birth date")
  val possibleDDateP = EOP("possible death date")
  val placeOfBirthP = EOP("place of birth")
  val placeOfDeathP = EOP("place of death")
  
  FOAF.Person.inModel(m).addProperty(SKOS.prefLabel, "Person","en")

  val dregex = """(\d{1,2})/(\d{1,2})/(\d{4})""".r
  
  def createDate(d : String): Resource = {
    val (bdate,edate) = 
    if (d.endsWith("-00-00")) {
        val year = d.substring(0,4)
        (s"${year}-01-01",s"${year}-12-31")
    } else if (d.endsWith("-00")) {
        val year = d.substring(0,4)
        val month = d.substring(5,7)
        val ldate = ISODateTimeFormat.yearMonth().parseDateTime(year+"-"+month).dayOfMonth().withMaximumValue().getDayOfMonth()
        (s"${year}-${month}-01",s"${year}-12-${ldate}")
    } else dregex.findFirstIn(d) match {
      case Some(dregex(m,d,year)) => 
        val month = if (m.length()==1) "0"+m else m
        val date = if (d.length()==1) "0"+d else d
        (s"${year}-${month}-${date}",s"${year}-${month}-${date}")
      case _ => 
        throw new IllegalArgumentException("Unknown date: "+d)
    }
    val date = I(s"${ns}date_${bdate}T00:00:00.000TO${edate}T23:59:59.999",d,CIDOC.TimeSpan)
    date.addProperty(CIDOC.begin_of_the_begin, bdate+"T00:00:00.000",XSDDatatype.XSDdateTime)
    date.addProperty(CIDOC.end_of_the_end, edate+"T23:59:59.999",XSDDatatype.XSDdateTime)
    date
  }
  
  def main(args: Array[String]): Unit = {
    val wr = CSVDictReader("procope.csv")
    var i = 1
    for (r <- wr) {
      val person = I(ns+"person_"+encode(r("Full Name")),r("Full Name"),if (r("GenderGroup")!="Group") CIDOC.Person else CIDOC.Group)
      r("Source").trim.split(',').map(s => s.trim).filter(s => !s.isEmpty).foreach(n => {
        val source = I(ns+"source_"+encode(n),n,Source)
        person.addProperty(sourceP,source)
      })
      r("EgoNetworksCombined").trim.split(',').map(s => s.trim).filter(s => !s.isEmpty).foreach(n => {
        val egonetwork = I(ns+"egonetwork_"+encode(n),n,EgoNetwork)
        person.addProperty(egonetworkP,egonetwork)
      })
      if (!r("EE ID").isEmpty) person.addProperty(eeIDP,r("EE ID"))
      r("Occupation").trim.split(',').map(s => s.trim).filter(s => !s.isEmpty).foreach(n => {
        val occupation = I(ns+"occupation_"+encode(n),Map("en"->n),Occupation)
        person.addProperty(occupationP,occupation)
      })
      if (!r("Political rank").isEmpty) {
        val politicalRank = I(ns+"politicalRank_"+encode(r("Political rank")),Map("en"->r("Political rank")),PoliticalRank)
        person.addProperty(politicalRankP,politicalRank)
      }
      if (!r("Military rank").isEmpty) {
        val militaryRank = I(ns+"militaryRank_"+encode(r("Military rank")),Map("en"->r("Military rank")),MilitaryRank)
        person.addProperty(militaryRankP,militaryRank)
      }
      if (!r("RecipientLetterCount by EE ID").isEmpty) person.addProperty(recipientLetterCountP,r("RecipientLetterCount by EE ID"),XSDDatatype.XSDinteger)
      if (!r("AuthorLetterCount by EE ID").isEmpty) person.addProperty(authorLetterCountP,r("AuthorLetterCount by EE ID"),XSDDatatype.XSDinteger)
      if (!r("Total Letters").isEmpty) person.addProperty(totalLetterCountP,r("Total Letters"),XSDDatatype.XSDinteger)
      if (!r("CNRS-D ID").isEmpty) person.addProperty(cnrsdIDP,r("CNRS-D ID"))
      if (!r("Full Name Reversed").isEmpty) person.addProperty(reversedNameP,r("Full Name Reversed"))
      if (!r("Particule?").isEmpty) person.addProperty(particuleP,r("Particule?"))
      if (!r("Relationships").isEmpty) person.addProperty(relationshipP,r("Relationships"))
      if (!r("Aristo title").isEmpty) {
        val title = I(ns+"title_"+encode(r("Aristo title")),Map("fr"->r("Aristo title")),AristocraticTitle)
        person.addProperty(titleP,title)
      }
      if (!r("Clerical title").isEmpty) {
        val title = I(ns+"title_"+encode(r("Clerical title")),Map("fr"->r("Clerical title")),ClericalTitle)
        person.addProperty(titleP,title)
      }
      if (!r("Wikipedia URL").isEmpty) {
        person.addProperty(wikipediaURLP,r("Wikipedia URL"))
        person.addProperty(OWL.sameAs,R(r("Wikipedia URL").replaceAll("http://en.wikipedia.org/wiki/","http://dbpedia.org/resource/")))
      }
      if (!r("VIAF ID").isEmpty) person.addProperty(viafIDP,r("VIAF ID"))
      if (!r("VIAF URL").isEmpty) person.addProperty(OWL.sameAs,R(r("VIAF URL")))
      r("Academies by VIAF").trim.split(';').map(s => s.trim).filter(s => !s.isEmpty).foreach(n => {
        val academy = I(ns+"academy_"+encode(n),Map("fr"->n),Academy)
        person.addProperty(academyP,academy)
      })
      if (!r("Wikipedia Image Link").isEmpty) person.addProperty(FOAF.depiction,R(r("Wikipedia Image Link")))
      r("Knowledge Network").trim.split(',').map(s => s.trim).filter(s => !s.isEmpty).foreach(n => {
        n.split('_').foldLeft(""){(l,r) =>
          if (!l.isEmpty) {
            val n1 = I(ns+"knowledgeNetwork_"+encode(l),Map("en"->l),KnowledgeNetwork)
            val n2 = I(ns+"knowledgeNetwork_"+encode(l+'_'+r),Map("en"->(l+" - "+r)),KnowledgeNetwork)
            n2.addProperty(RDFS.subClassOf,n1)
            l+'_'+r
          }
          r
        }
        val network = I(ns+"knowledgeNetwork_"+encode(n),Map("en"->n),KnowledgeNetwork)
        person.addProperty(knowledgeNetworkP,network)
      })
      r("Social Network").trim.split(',').map(s => s.trim).filter(s => !s.isEmpty).foreach(n => {
        n.split('_').foldLeft(""){(l,r) =>
          if (!l.isEmpty) {
            val n1 = I(ns+"socialNetwork_"+encode(l),Map("en"->l),SocialNetwork)
            val n2 = I(ns+"socialNetwork_"+encode(l+'_'+r),Map("en"->(l+" - "+r)),SocialNetwork)
            n2.addProperty(RDFS.subClassOf,n1)
            l+'_'+r
          }
          r
        }
        val network = I(ns+"socialNetwork_"+encode(n),Map("en"->n),SocialNetwork)
        person.addProperty(socialNetworkP,network)
      })
      r("Professional Network").trim.split(',').map(s => s.trim).filter(s => !s.isEmpty).foreach(n => {
        n.split('_').foldLeft(""){(l,r) =>
          if (!l.isEmpty) {
            val n1 = I(ns+"professionalNetwork_"+encode(l),Map("en"->l),ProfessionalNetwork)
            val n2 = I(ns+"professionalNetwork_"+encode(l+'_'+r),Map("en"->(l+" - "+r)),ProfessionalNetwork)
            n2.addProperty(RDFS.subClassOf,n1)
            l+'_'+r
          }
          r
        }
        val network = I(ns+"professionalNetwork_"+encode(n),Map("en"->n),ProfessionalNetwork)
        person.addProperty(professionalNetworkP,network)
      })
      r("Religious Network").trim.split(',').map(s => s.trim).filter(s => !s.isEmpty).foreach(n => {
        n.split('_').foldLeft(""){(l,r) =>
          if (!l.isEmpty) {
            val n1 = I(ns+"religiousNetwork_"+encode(l),Map("en"->l),ReligiousNetwork)
            val n2 = I(ns+"religiousNetwork_"+encode(l+'_'+r),Map("en"->(l+" - "+r)),ReligiousNetwork)
            n2.addProperty(RDFS.subClassOf,n1)
            l+'_'+r
          }
          r
        }
        val network = I(ns+"religiousNetwork_"+encode(n),Map("en"->n),ReligiousNetwork)
        person.addProperty(religiousNetworkP,network)
      })
      r("Academies").trim.split(',').map(s => s.trim).filter(s => !s.isEmpty).foreach(n => {
        val academy = I(ns+"academy_"+encode(n),Map("fr"->n),Academy)
        person.addProperty(academyP,academy)
      })
      r("Salons").trim.split(',').map(s => s.trim).filter(s => !s.isEmpty).foreach(n => {
        val salon = I(ns+"salon_"+encode(n),Map("fr"->n),Salon)
        person.addProperty(salonP,salon)
      })
      if (!r("GenderGroup").isEmpty) {
        (r("GenderGroup")) match {
          case "Male" => person.addProperty(FOAF.gender,SDMXCode.sexMale)
          case "Female" => person.addProperty(FOAF.gender,SDMXCode.sexFemale)
          case "Group" =>
          case _ => println("Unknown gender")
        }
      }
      if (!r("Nationality").isEmpty) {
        if (r("Nationality").endsWith("?"))
          r("Nationality").substring(0,r("Nationality").length-1).trim.split('-').map(s => s.trim).filter(s => !s.isEmpty).foreach(n => {
            val nationality = I(ns+"nationality_"+encode(n),Map("en"->n),Nationality)
            person.addProperty(possibleNationalityP,nationality)
          })
        else r("Nationality").trim.split('-').map(s => s.trim).filter(s => !s.isEmpty).foreach(n => {
            val nationality = I(ns+"nationality_"+encode(n),Map("en"->n),Nationality)
            person.addProperty(possibleNationalityP,nationality)
          })
      }
      if (!r("Birth Date").isEmpty) {
        if (r("Birth Date").endsWith(" ?"))
          person.addProperty(possibleBDateP,createDate(r("Birth Date").substring(0,r("Birth Date").length()-2)))
        else person.addProperty(bdateP,createDate(r("Birth Date")))
      }
      if (!r("Death Date").isEmpty) {
        if (r("Death Date").endsWith(" ?"))
          person.addProperty(possibleDDateP,createDate(r("Death Date").substring(0,r("Death Date").length()-2)))
        else person.addProperty(ddateP,createDate(r("Death Date")))
      }
      if (!r("Birth City").isEmpty) {
        val city = I(ns+"location_"+encode(r("Birth City").trim),Map("en"->r("Birth City").trim),City)
        person.addProperty(placeOfBirthP,city)
        if (!r("Birth Country").isEmpty) {
          val country = I(ns+"location_"+encode(r("Birth Country").trim),Map("en"->r("Birth Country").trim),Country)
          city.addProperty(CIDOC.place_falls_within,country)
        }
      } else if (!r("Birth Country").isEmpty) {
        val country = I(ns+"location_"+encode(r("Birth Country").trim),Map("en"->r("Birth Country").trim),Country)
        person.addProperty(placeOfBirthP,country)
      }
      if (!r("Death City").isEmpty) {
        val city = I(ns+"location_"+encode(r("Death City").trim),Map("en"->r("Death City").trim),City)
        person.addProperty(placeOfDeathP,city)
        if (!r("Death Country").isEmpty) {
          val country = I(ns+"location_"+encode(r("Death Country").trim),Map("en"->r("Death Country").trim),Country)
          city.addProperty(CIDOC.place_falls_within,country)
        }
      } else if (!r("Death Country").isEmpty) {
        val country = I(ns+"location_"+encode(r("Death Country").trim),Map("en"->r("Death Country").trim),Country)
        person.addProperty(placeOfDeathP,country)
      }
      /*Source,EgoNetworksCombined,EE ID,Occupation,Political rank,Military rank,
      RecipientLetterCount by EE ID,AuthorLetterCount by EE ID,Total Letters,CNRS-D ID,
      Full Name,Full Name Reversed,Particule?,Relationships,Aristo title,Clerical title,
      Wikipedia URL,VIAF ID,VIAF URL,Academies by VIAF,Wikipedia Image Link,
      Knowledge Network,Social Network,Professional Network,Religious Network,
      Academies,Royal Academy,Institut de France,Academy_single,
      Academiy_single_provincial,Salons,GenderGroup,Nationality,
      Birth Date,Birth Year,Death Date,Death Year,Birth City,Birth Country,
      Death City,Death Country,
      LockeNet,ASmithNet,VoltaireNet,RousseauNet,D'AlembertNet,HumeNet,BenthamNet*/
    }
    m.setNsPrefixes(PrefixMapping.Standard)
    m.setNsPrefix("crm",CIDOC.ns)
    m.setNsPrefix("skos",SKOS.ns)
    m.setNsPrefix("foaf",FOAF.NS)
    m.setNsPrefix("procope",ns)
    m.setNsPrefix("procopes",sns)

    RDFDataMgr.write(new FileOutputStream("procope.ttl"), m, RDFFormat.TTL)
  }
}
