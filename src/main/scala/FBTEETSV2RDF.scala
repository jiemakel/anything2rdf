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
import scala.collection.mutable.HashSet
import org.apache.jena.vocabulary.DCTerms
import scala.collection.mutable.HashMap
import org.apache.jena.rdf.model.Property
import org.apache.jena.datatypes.xsd.XSDDatatype

object FBTEETSV2RDF extends Anything2RDF {

  val sns = "http://ldf.fi/fbtee-schema#"
  val ns = "http://ldf.fi/fbtee/"
  
  val LowerTerritory = EC("Lower Territory")
  val SovereignTerritory = EC("Sovereign Territory")
  
  val AdministrativeArea = EC("Administrative Area")
  val Country = EC("Country")

  val Zone = EC("Geographic Zone")
  val BSR = EC("BSR")
  val PlaceGrouping = EC("Place Grouping")
  
  val Address = EC("Address")

  LowerTerritory.addProperty(RDFS.subClassOf, CIDOC.Place)
  SovereignTerritory.addProperty(RDFS.subClassOf, CIDOC.Place)
  AdministrativeArea.addProperty(RDFS.subClassOf, CIDOC.Place)
  Country.addProperty(RDFS.subClassOf, CIDOC.Place)
  Zone.addProperty(RDFS.subClassOf, CIDOC.Place)
  BSR.addProperty(RDFS.subClassOf, CIDOC.Place)
  PlaceGrouping.addProperty(RDFS.subClassOf, CIDOC.Place)
  Address.addProperty(RDFS.subClassOf, CIDOC.Place)
  
  val distanceFromNeuchatel = EDP("distance from Neuchâtel")
  
  val HRE = I(ns+"place_grouping_HRE",Map("en"->"The Holy Roman Empire"), PlaceGrouping)
  val EL = I(ns+"place_grouping_EL",Map("en"->"Ecclesiastical Lands"), PlaceGrouping)
  val IFC = I(ns+"place_grouping_IFC",Map("en"->"Imperial Free Cities"), PlaceGrouping)
  val P = I(ns+"place_grouping_P",Map("en"->"French Parliaments 1776-1790"), PlaceGrouping)
  val HE = I(ns+"place_grouping_HE",Map("en"->"Higher Education Towns"), PlaceGrouping)
  val HT = I(ns+"place_grouping_HT",Map("en"->"Habsburg Territories"), PlaceGrouping)
  val WT = I(ns+"place_grouping_WT",Map("en"->"Wurttenberg Territories"), PlaceGrouping)
  val PT = I(ns+"place_grouping_PT",Map("en"->"Papal Territories"), PlaceGrouping)
  val PrT = I(ns+"place_grouping_PrT",Map("en"->"Prussian Territories"), PlaceGrouping)

  val authorWorkProps = Map(
    "primary" -> EOP("primary author"),
    "secondary" -> EOP("secondary author"),
    "translator" -> EOP("translator"),
    "editor" -> EOP("editor"))

  val uncertainAuthorWorkProps = Map(
    "primary" -> EOP("possible primary author"),
    "secondary" -> EOP("possible secondary author"),
    "translator" -> EOP("possible translator"),
    "editor" -> EOP("possible editor"))

  val callNumber = EDP("call number")

  val Catalogue = EC("Catalogue")
  val catalogue = EOP("catalogue")

  val editionTypeStatusMap = Map(
      "Certain" -> EOP("edition type"),
      "Probable" -> EOP("probable edition type"),
      "Pseudo" -> EOP("pseudo edition type"),
      "Unreviewed" -> EOP("unreviewed edition type"))
      
  val pages = EDP("number of pages")
  val pagesText = EDP("number of pages in original format")
  val volumes = EDP("number of volumes")
  val section = EDP("section")
  val Format = EC("Format")
  val format = EOP("format")
  val sheets = EDP("sheets")
  
  val Publisher = EC("Publisher")
  val statedPublisher = EOP("stated publisher")
  val possiblePublisher = EOP("possible publisher")
  
  val statedPlaceOfPublication = EOP("stated place of publication")
  val possiblePlaceOfPublication = EOP("possible place of publication")
  
  val statedYearOfPublication = EOP("stated year of publication")
  val possibleYearOfPublication = EOP("possible year of publication")
  
  val pageNumber = EDP("page number")
  
  val accountHeading = EDP("account heading")
  
  val volumeExchanged = EDP("volume exchanged")
  val numberOfCopies = EDP("number of copies")
  val volumeNotExchanged = EDP("volume not exchanged")
      
  /*    1 Arabic
   3 Dutch
 275 English
   1 English-Farsi
   1 English-Italian
   3 English-Latin
   7 French
 115 German
   6 Greek
   1 Greek-Chinese
   2 Ionic Greek
  58 Italian
  45 Latin
   1 Latin-English
   1 Latin-French
   2 Portuguese
   3 Russian
  14 Spanish
   4 Swedish */
  val langMap = Map(
      "Arabic" -> "ar",
      "Dutch" -> "nl",
      "English" -> "en",
      "English-Farsi" -> "en-fa",
      "English-Italian" -> "en-it",
      "English-Latin" -> "en-la",
      "French" -> "fr",
      "German" -> "de",
      "Greek" -> "gr",
      "Greek-Chinese" -> "gr-zh",
      "Ionic Greek" -> "grc-ion",
      "Italian" -> "it",
      "Latin" -> "la",
      "Latin-English" -> "la-en",
      "Latin-French" -> "la-fr",
      "Portuguese" -> "pt",
      "Russian" -> "ru",
      "Spanish" -> "es",
      "Swedish" -> "sv")
      
  val langIRIMap = Map(
      "Dutch" -> R("http://lexvo.org/id/iso639-3/nld"),
      "English" -> R("http://lexvo.org/id/iso639-3/eng"),
      "French" -> R("http://lexvo.org/id/iso639-3/fra"),
      "German" -> R("http://lexvo.org/id/iso639-3/deu"),
      "Greek" -> R("http://lexvo.org/id/iso639-3/eng"),
      "Italian" -> R("http://lexvo.org/id/iso639-3/ell"),
      "Latin" -> R("http://lexvo.org/id/iso639-3/lat"),
      "Spanish" -> R("http://lexvo.org/id/iso639-3/spa")
      )
      
  val genderMap = Map("F"->SDMXCode.sexFemale,"M"->SDMXCode.sexMale,"mixed"->SDMXCode.sexNotApplicable)
  
  val hasCorrespondence = EDP("has correspondence")
  
  val EditionType = EC("Edition Type")

  val Tag = EC("Tag")
  val tag = EOP("tag")
  
  val agent = EOP("agent")
  val middleman = EOP("middleman")
  val sentVia = EOP("sent via")

  val Keyword = EC("Keyword")
  
  val balleNumber = EDP("balle number")
  
  val Order = EC("Order")
  val Transaction = EC("Transaction")
  
  val cash = EDP("cash")
  val client = EOP("client")

  val Profession = EC("Profession")
  val profession = EOP("profession")
  
  val designation = EDP("designation")
  val status = EOP("status")
  val Status = EC("Status")
  
  val deathDate = EOP("date of death")
  val birthDate = EOP("date of birth")
  val ParisianKeyword = EC("Parisian Keyword")
  
  val illegality = EDP("illegality")
  
  val clientType = EOP("client type")
  val ClientType = EC("Client Type")
  
  val letters = EDP("letters")
  val documents = EDP("documents")
  val active = EOP("active")
  
  val correspondenceManuscripts = EDP("manuscripts")
  val correspondencePlace = EOP("place of correspondence")
  
  val sourceType = EOP("source type")
  val SourceType = EC("Source Type")
  
  val hasPossibleTimeSpan = EOP("has possible time-span")
  
  override def ANE(r: Resource, p: Property, o : String) : Unit = {
    if (!o.trim.isEmpty() && o!="N") r.addProperty(p,o)
  }

  override def ANE(r: Resource, p: Property, o : String, l : String) : Unit = {
    if (!o.trim.isEmpty() && o!="N") r.addProperty(p,o,l)
  }

  def makeTimeSpan(s: String): Resource = {
    val ends = s.split("-")
    val (bob,_) = makeDateTime(ends(0), "", "")
    val (_,eoe) = makeDateTime(ends(ends.length-1),"","")
    return makeTimeSpan(s, bob, eoe)
  }
  
  val monthMap = Map(
      "January" -> "01",
      "February" -> "02",
      "March" -> "03",
      "April" -> "04",
      "May" -> "05",
      "June" -> "06",
      "July" -> "07",
      "August" -> "08",
      "September" -> "09",
      "October" -> "10",
      "November" -> "11",
      "December" -> "12")
      
  val lastDateMap = Map(
      "January" -> "31",
      "February" -> "28",
      "March" -> "31",
      "April" -> "30",
      "May" -> "31",
      "June" -> "30",
      "July" -> "31",
      "August" -> "31",
      "September" -> "30",
      "October" -> "31",
      "November" -> "30",
      "December" -> "31")
  
  def makeDate(s: String, earliest: Boolean = true): String = {
    val parts = s.split(" ")
    parts.length match {
      case 3 => parts(2)+"-"+monthMap(parts(1))+"-"+parts(0).replace("th","").replace("st","").replace("rd","").replace("nd","")
      case 2 => parts(1)+"-"+monthMap(parts(0))+"-"+(if (earliest) "01" else lastDateMap(parts(0)))
      case 1 => parts(0)+"-"+(if (earliest) "01-01" else "12-31")
    }
  }
  
  def main(args: Array[String]): Unit = {
    var wr = CSVReader("fbtee/authors.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) I(ns + "author_" + r(0), r(1), CIDOC.Person)
    /*     
     `book_code` char(9) NOT NULL,
  `author_code` char(9) NOT NULL,
  `author_type` varchar(10) NOT NULL DEFAULT 'primary',
  `certain` bit(1) NOT NULL,
     */
    wr = CSVReader("fbtee/books_authors.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) {
      val b = R(ns + "manifestation_" + r(0))
      if (r(3).charAt(0) == 1) b.addProperty(authorWorkProps(r(2)), R(ns + "author_" + r(1)))
      else b.addProperty(uncertainAuthorWorkProps(r(2)), R(ns + "author_" + r(1)))
    }

    /*   `book_code` char(9) NOT NULL,
  `call_number` varchar(300) NOT NULL, */
    wr = CSVReader("fbtee/books_call_numbers.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) R(ns + "manifestation_" + r(0)).addProperty(callNumber, r(1))

    /* CREATE TABLE `books_stn_catalogues` (
  `book_code` char(9) NOT NULL,
  `catalogue` varchar(200) NOT NULL,
      */
    wr = CSVReader("fbtee/books_stn_catalogues.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) R(ns + "manifestation_" + r(0)).addProperty(catalogue, I(ns + "catalogue_" + encode(r(1)), r(1), Catalogue))

    /* 
     * CREATE TABLE `books` (
  `book_code` char(9) NOT NULL,
  `super_book_code` char(11) NOT NULL,
  `edition_status` varchar(15) NOT NULL,
  `edition_type` varchar(50) NOT NULL,
  `full_book_title` varchar(750) DEFAULT NULL,
  `short_book_titles` varchar(1000) DEFAULT NULL,
  `translated_title` varchar(750) DEFAULT NULL,
  `translated_language` varchar(50) DEFAULT NULL,
  `languages` varchar(200) DEFAULT NULL,
  `stated_publishers` varchar(1000) DEFAULT NULL,
  `actual_publishers` varchar(1000) DEFAULT NULL,
  `stated_publication_places` varchar(1000) DEFAULT NULL,
  `actual_publication_places` varchar(1000) DEFAULT NULL,
  `stated_publication_years` varchar(1000) DEFAULT NULL,
  `actual_publication_years` varchar(10) DEFAULT NULL,
  `pages` varchar(250) DEFAULT NULL,
  `quick_pages` varchar(10) DEFAULT NULL,
  `number_of_volumes` int(11) DEFAULT NULL,
  `section` varchar(10) DEFAULT NULL,
  `edition` varchar(100) DEFAULT NULL,
  `book_sheets` varchar(200) DEFAULT NULL,
  `notes` varchar(4000) DEFAULT NULL,
  `research_notes` varchar(1000) DEFAULT NULL,
     * 
     */
    wr = CSVReader("fbtee/books.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) {
      val b = R(ns + "manifestation_" + r(0))
      b.addProperty(RDF.`type`, FRBR.Manifestation_Product_Type)
      b.addProperty(FRBR.expression_realises_work, R(ns + "work_" + r(1)))
      b.addProperty(editionTypeStatusMap(r(2)), I(ns+"edition_type_"+encode(r(3)),Map("en" -> r(3)),EditionType))
      ANE(b, SKOS.prefLabel, r(4), "fr")
      if (r(5)!="N") for (title <- r(5).split(";")) b.addProperty(SKOS.altLabel, title.trim, "fr")
      if (r(6)!="N") if (r(7)=="N") b.addProperty(SKOS.prefLabel, r(6)) else b.addProperty(SKOS.prefLabel, r(6), langMap(r(7)))
      if (r(8)!="N") for (lang <- r(8).split(",")) b.addProperty(DCTerms.language, langIRIMap(lang.trim))
      if (r(9)!="N") if (r(10)!="N") b.addProperty(statedPublisher, I(ns+"publisher_"+encode(r(9)),r(9),Publisher)) else b.addProperty(DCTerms.publisher, I(ns+"publisher_"+encode(r(9)),r(9),Publisher))
      if (r(10)!="N") if (r(10).endsWith("?")) b.addProperty(possiblePublisher, I(ns+"publisher_"+encode(r(10).substring(0, r(10).length - 1)),r(10).substring(0, r(10).length - 1),Publisher)) else b.addProperty(DCTerms.publisher, I(ns+"publisher_"+encode(r(10)),r(10),Publisher))
      if (r(11)!="n.pl.") for (placeOfPublication <- r(11).split(";")) if (r(12)!="?") b.addProperty(statedPlaceOfPublication, I(ns+"place_"+encode(placeOfPublication.trim),placeOfPublication.trim,CIDOC.Place)) else b.addProperty(CIDOC.took_place_at, I(ns+"place_"+encode(placeOfPublication.trim),placeOfPublication.trim,CIDOC.Place))
      if (r(12)!="?") for (placeOfPublication <- r(12).split(";")) if (placeOfPublication.endsWith("?")) b.addProperty(possiblePlaceOfPublication, I(ns+"place_"+encode(placeOfPublication.trim.substring(0, placeOfPublication.trim.length - 1)),placeOfPublication.trim.substring(0, placeOfPublication.trim.length - 1),CIDOC.Place)) else b.addProperty(CIDOC.took_place_at, I(ns+"place_"+encode(placeOfPublication.trim),placeOfPublication.trim,CIDOC.Place))
      if (r(13)!="N" && r(13)!="n.d.") if (r(14)!="N") b.addProperty(statedYearOfPublication, makeTimeSpan(r(13)))
        else {
          if (r(13)=="1780/1782") {
            b.addProperty(CIDOC.has_timeSpan, makeTimeSpan("1780"))
            b.addProperty(CIDOC.has_timeSpan, makeTimeSpan("1782"))
          } else if (r(13).endsWith("?"))
            b.addProperty(hasPossibleTimeSpan, makeTimeSpan(r(13).substring(0,r(13).length-1)))
          else b.addProperty(CIDOC.has_timeSpan, makeTimeSpan(r(13)))
      }
      if (r(14)!="N") if (r(14).endsWith("?")) b.addProperty(possibleYearOfPublication, makeTimeSpan(r(14).substring(0, r(14).length - 1))) else b.addProperty(CIDOC.has_timeSpan, makeTimeSpan(r(14)))
      ANE(b, pagesText, r(15))
      if (r(16)!="N") b.addProperty(pages,r(16),XSDDatatype.XSDinteger)
      b.addProperty(volumes, r(17),XSDDatatype.XSDinteger)
      if (r(18)!="N") b.addProperty(section, r(18), XSDDatatype.XSDinteger)
      if (r(19)!="N") b.addProperty(format, I(ns+"format_"+encode(r(19)),r(19), Format))
      ANE(b, sheets, r(20))
      ANE(b, RDFS.comment, r(21), "en")
      ANE(b, SKOS.scopeNote, r(22),"en")
    }
    
    /*CREATE TABLE `people` (
  `person_code` char(6) NOT NULL,
  `person_name` varchar(155) DEFAULT NULL,
  `sex` char(1) DEFAULT NULL,
  `title` varchar(50) DEFAULT NULL,
  `other_names` varchar(1000) DEFAULT NULL,
  `designation` varchar(50) DEFAULT NULL,
  `status` varchar(50) DEFAULT NULL,
  `birth_date` varchar(20) DEFAULT NULL,
  `death_date` varchar(20) DEFAULT NULL,
  `notes` varchar(4000) DEFAULT NULL,
*/
    val personNameMap = new HashMap[String,String]
    wr = CSVReader("fbtee/people.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) {
      personNameMap.put(r(0),r(1))
      val p = I(ns + "person_" + r(0),r(1),CIDOC.Person)
      p.addProperty(FOAF.gender,genderMap(r(2)))
      ANE(p,FOAF.title,r(3))
      ANE(p,SKOS.altLabel, r(4))
      ANE(p,designation, r(5))
      if (r(6)!="N") p.addProperty(status, I(ns+"status_"+encode(r(6)),Map("en"->r(6)),Status))
      if (r(7)!="") if (r(7).contains(" ")) {
        val d = makeDate(r(7))
        p.addProperty(birthDate, makeTimeSpan(r(7),d+"T00:00:00",d+"T23:59:59"))
      } else p.addProperty(birthDate, makeTimeSpan(r(7)))
      if (r(8)!="" && r(8)!="18  ") if (r(8).contains(" ")) {
        val d = makeDate(r(8))
        p.addProperty(deathDate, makeTimeSpan(r(8),d+"T00:00:00",d+"T23:59:59"))
      } else p.addProperty(deathDate, makeTimeSpan(r(8)))
      ANE(p,RDFS.comment,r(9))
    }
    

   /* CREATE TABLE `clients_people` (
  `client_code` char(6) NOT NULL,
  `person_code` char(6) NOT NULL,
  PRIMARY KEY (`client_code`,`person_code`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;
     */
    val clientToNumberOfPeopleMap = new HashMap[String,Int]
    val peopleToNumberOfClientsMap = new HashMap[String,Int]
    wr = CSVReader("fbtee/clients_people.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) {
      clientToNumberOfPeopleMap.put(r(0),clientToNumberOfPeopleMap.getOrElse(r(0), 0) + 1)
      peopleToNumberOfClientsMap.put(r(1),peopleToNumberOfClientsMap.getOrElse(r(1), 0) + 1)
    }
    wr = CSVReader("fbtee/clients_people.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    val clientToPossiblePersonMap = new HashMap[String,String]
    for (r <- wr) {
      if (clientToNumberOfPeopleMap(r(0)) == 1 && peopleToNumberOfClientsMap(r(1)) == 1) clientToPossiblePersonMap.put(r(0),r(1))
      else R(ns + "client_" + r(0)).addProperty(CIDOC.has_current_or_former_member,  R(ns + "person_" + r(0)))
    }

    
    /* CREATE TABLE `clients` (
  `client_code` char(6) NOT NULL,
  `client_name` varchar(100) DEFAULT NULL,
  `has_correspondence` bit(1) DEFAULT NULL,
  `partnership` bit(1) DEFAULT NULL,
  `gender` varchar(5) DEFAULT NULL,
  `data_source` varchar(25) NOT NULL,
  `option_menu_type` varchar(25) NOT NULL,
  `number_of_letters` smallint(6) DEFAULT NULL,
  `number_of_documents` smallint(6) DEFAULT NULL,
  `first_date` varchar(20) DEFAULT NULL,
  `last_date` varchar(20) DEFAULT NULL,
  `notes` varchar(4000) DEFAULT NULL, */
    val clientToResourceMap = new HashMap[String,Resource]
    val clientNameMap = new HashMap[String,String]
    wr = CSVReader("fbtee/clients.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) {
      val c = if (clientToPossiblePersonMap.contains(r(0))) {
        val p = clientToPossiblePersonMap(r(0))
        if (personNameMap(p) == r(1) && r(3).charAt(0) == 0) R(ns + "person_" + p) else
          I(ns+"client_"+r(0),r(1),CIDOC.Group).addProperty(CIDOC.has_current_or_former_member, R(ns + "person_" + p))
      } else I(ns+"client_"+r(0),r(1),CIDOC.Group)
      clientToResourceMap.put(r(0),c)
      clientNameMap.put(r(0),r(1))
      c.addProperty(hasCorrespondence, if (r(2).charAt(0) == 0) "false" else "true",XSDDatatype.XSDboolean)
      if (r(4)!="N") c.addProperty(FOAF.gender, genderMap(r(4)))
      c.addProperty(DCTerms.source, I(ns+"source_"+encode(r(5)),Map("en"->r(5)),CIDOC.Document))
      c.addProperty(clientType, I(ns+"clientType_"+encode(r(6)),Map("en"->r(6)),ClientType))
      if (r(7)!="0") c.addProperty(letters, r(7), XSDDatatype.XSDinteger)
      if (r(8)!="0") c.addProperty(documents, r(8), XSDDatatype.XSDinteger)
      val bob = if (r(9)!="N" && r(9)!="6th June " && r(9)!="26th 1781" && r(9)!="4th May " && r(9)!="22nd May " && r(9)!="7th July ") makeDate(r(9),true)+"T00:00:00" else ""
      val eoe = if (r(10)!="N") makeDate(r(10),false)+"T23:59:59" else ""
      if (bob!="" || eoe!="")
        c.addProperty(active, makeTimeSpan(r(9) + " - " + r(10), bob,eoe))
      ANE(c, RDFS.comment, r(11))
    }
    
    /* CREATE TABLE `places` (
  `place_code` char(5) NOT NULL,
  `name` varchar(50) NOT NULL,
  `alternative_names` varchar(200) DEFAULT NULL,
  `town` varchar(50) DEFAULT NULL,
  `C18_lower_territory` varchar(50) DEFAULT NULL,
  `C18_sovereign_territory` varchar(50) DEFAULT NULL,
  `C21_admin` varchar(50) DEFAULT NULL,
  `C21_country` varchar(50) DEFAULT NULL,
  `geographic_zone` varchar(50) DEFAULT NULL,
  `BSR` varchar(50) DEFAULT NULL,
  `HRE` bit(1) DEFAULT NULL,
  `EL` bit(1) DEFAULT NULL,
  `IFC` bit(1) DEFAULT NULL,
  `P` bit(1) NOT NULL,
  `HE` bit(1) NOT NULL,
  `HT` bit(1) NOT NULL,
  `WT` bit(1) NOT NULL,
  `PT` bit(1) NOT NULL,
  `PrT` bit(1) NOT NULL,
  `distance_from_neuchatel` int(11) DEFAULT NULL,
  `latitude` decimal(10,8) DEFAULT NULL,
  `longitude` decimal(10,8) DEFAULT NULL,
  `notes` varchar(1000) DEFAULT NULL,
     */
    val placeNameMap = new HashMap[String,String]
    wr = CSVReader("fbtee/places.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) {
      val p = I(ns+"place_"+r(0),r(1), CIDOC.Place)
      placeNameMap.put(r(0),r(1))
      if (r(2)!="N") for (name <- r(2).split(",")) p.addProperty(SKOS.altLabel, name.trim)
      p.addProperty(SKOS.altLabel,r(3))
      val c18_lt = I(ns+"c18_lower_territory_"+encode(r(4)),r(4), LowerTerritory)
      p.addProperty(CIDOC.place_falls_within,c18_lt)
      val c18_st = I(ns+"c18_sovereign_territory_"+encode(r(5)),r(5),SovereignTerritory)
      c18_lt.addProperty(CIDOC.place_falls_within,c18_st)
      val c21_admin = I(ns+"c21_admin_"+encode(r(6)),r(6), AdministrativeArea)
      p.addProperty(CIDOC.place_falls_within,c21_admin)
      val c21_country = I(ns+"c21_country_"+encode(r(7)),r(7),Country)
      c21_admin.addProperty(CIDOC.place_falls_within,c21_country)
      val zone = I(ns+"zone_"+encode(r(8)),Map("en"->r(8)),Zone)
      val bsr = I(ns+"bsr_"+encode(r(9)),Map("en"->r(9)),BSR)
      c18_st.addProperty(CIDOC.place_falls_within, zone)
      c21_country.addProperty(CIDOC.place_falls_within, zone)
      c18_st.addProperty(CIDOC.place_falls_within, bsr)
      c21_country.addProperty(CIDOC.place_falls_within, bsr)
      if (r(10).charAt(0) == 1) p.addProperty(CIDOC.place_falls_within, HRE)
      if (r(11).charAt(0) == 1) p.addProperty(CIDOC.place_falls_within, EL)
      if (r(12).charAt(0) == 1) p.addProperty(CIDOC.place_falls_within, IFC)
      if (r(13).charAt(0) == 1) p.addProperty(CIDOC.place_falls_within, P)
      if (r(14).charAt(0) == 1) p.addProperty(CIDOC.place_falls_within, HE)
      if (r(15).charAt(0) == 1) p.addProperty(CIDOC.place_falls_within, HT)
      if (r(16).charAt(0) == 1) p.addProperty(CIDOC.place_falls_within, WT)
      if (r(17).charAt(0) == 1) p.addProperty(CIDOC.place_falls_within, PT)
      if (r(18).charAt(0) == 1) p.addProperty(CIDOC.place_falls_within, PrT)
      p.addProperty(distanceFromNeuchatel,r(19),XSDDatatype.XSDinteger)
      p.addProperty(WGS84.lat, r(20), XSDDatatype.XSDdecimal)
      p.addProperty(WGS84.long, r(21), XSDDatatype.XSDdecimal)
      ANE(p,RDFS.comment,r(22))
    }

    /* CREATE TABLE `clients_addresses` (
  `client_code` char(6) NOT NULL,
  `place_code` char(5) NOT NULL,
  `address` varchar(50) DEFAULT NULL,
     */
    wr = CSVReader("fbtee/clients_addresses.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) {
      val p = if (r(2)!="N")
        I(ns+"place_"+r(1)+"_"+encode(r(2)),r(2)+", "+placeNameMap(r(1)),Address).addProperty(CIDOC.place_falls_within, R(ns+"place_"+r(0)))
      else R(ns+"place_"+r(0))
      clientToResourceMap(r(0)).addProperty(ORG.hasSite, p)
    }
    

    /* CREATE TABLE `clients_correspondence_manuscripts` (
  `client_code` char(6) NOT NULL,
  `position` int(11) NOT NULL,
  `manuscript_numbers` varchar(500) DEFAULT NULL, */
    wr = CSVReader("fbtee/clients_correspondence_manuscripts.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) clientToResourceMap(r(0)).addProperty(correspondenceManuscripts,r(1))

    /* CREATE TABLE `clients_correspondence_places` (
  `client_code` char(6) NOT NULL,
  `place_code` char(5) NOT NULL,
  `from_date` varchar(20) DEFAULT NULL,
     */
    wr = CSVReader("fbtee/clients_correspondence_places.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) {
      clientToResourceMap(r(0)).addProperty(correspondencePlace, R(ns + "place_" + r(1)))
      if (r(2)!="N") {
        val s = m.createResource()
        s.addProperty(RDF.`type`,RDF.Statement)
        s.addProperty(RDF.subject, clientToResourceMap(r(0)))
        s.addProperty(RDF.predicate, correspondencePlace)
        s.addProperty(RDF.`object`, R(ns + "place_" + r(1)))
        val d = makeDate(r(2))
        s.addProperty(CIDOC.has_timeSpan, makeTimeSpan("From " + r(2),Some(d+"T00:00:00"),Some(d+"T23:59:59"),None,None))
      }
    }

    /* CREATE TABLE `clients_professions` (
  `client_code` char(6) NOT NULL,
  `profession_code` char(5) NOT NULL,
  PRIMARY KEY (`client_code`,`profession_code`)*/
    wr = CSVReader("fbtee/clients_professions.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) clientToResourceMap(r(0)).addProperty(profession, R(ns + "profession_" + r(1)))

    /*CREATE TABLE `keywords` (
  `keyword_code` char(5) NOT NULL,
  `keyword` varchar(250) NOT NULL,
  `definition` varchar(1000) DEFAULT NULL,
  `tag_code` char(5) NOT NULL DEFAULT '',*/
    val keywordNameCodeMap = new HashMap[String, Resource]
    wr = CSVReader("fbtee/keywords.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) {
      val kw = I(ns + "keyword_" + r(0), Map("en" -> r(1)), Keyword)
      kw.addProperty(DCTerms.description, r(2), "en")
      kw.addProperty(tag, R(ns + "tag_" + r(3)))
      keywordNameCodeMap.put(r(1), kw)
    }

    /* CREATE TABLE `keyword_assignments` (
  `super_book_code` char(11) NOT NULL,
  `position` int(11) NOT NULL,
  `keyword` varchar(250) NOT NULL, */
    wr = CSVReader("fbtee/keyword_assignments.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr)
      R(ns + "work_" + r(0)).addProperty(DCTerms.subject, keywordNameCodeMap.getOrElseUpdate(r(2), I(ns + "keyword_" + encode(r(2)), Map("en" -> r(2)), Keyword)))
    /*
CREATE TABLE `keyword_free_associations` (
  `keyword` varchar(250) NOT NULL,
  `association` varchar(250) NOT NULL*/
    wr = CSVReader("fbtee/keyword_free_associations.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) keywordNameCodeMap.getOrElseUpdate(r(0), I(ns + "keyword_" + encode(r(0)), Map("en" -> r(0)), Keyword)).addProperty(SKOS.related, keywordNameCodeMap.getOrElseUpdate(r(1), I(ns + "keyword_" + encode(r(1)), Map("en" -> r(1)), Keyword)))

    /*CREATE TABLE `keyword_tree_associations` (
  `keyword` varchar(250) NOT NULL,
  `association` varchar(250) NOT NULL
*/
    wr = CSVReader("fbtee/keyword_tree_associations.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) keywordNameCodeMap(r(0)).addProperty(SKOS.broader, keywordNameCodeMap(r(1)))

    /*CREATE TABLE `orders` (
  `order_code` char(9) NOT NULL,
  `client_code` char(6) DEFAULT NULL,
  `place_code` char(5) DEFAULT NULL,
  `date` int(11) DEFAULT NULL,
  `manuscript_number` varchar(50) DEFAULT NULL,
  `manuscript_type` varchar(50) DEFAULT NULL,
  `balle_number` varchar(50) DEFAULT NULL,
  `cash` bit(1) DEFAULT NULL,
  */
    val orderNameMap = new HashMap[String,String]
    wr = CSVReader("fbtee/orders.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) {
      val o = R(ns+"order_"+r(0))
      o.addProperty(RDF.`type`,Order)
      if (r(1)!="N") o.addProperty(client,clientToResourceMap(r(1)))
      if (r(2)!="N") o.addProperty(CIDOC.took_place_at,R(ns+"place_"+r(2)))
        val date = r(3).substring(0,4)+"-"+r(3).substring(4,6)+"-"+r(3).substring(6)
        o.addProperty(CIDOC.has_timeSpan,makeTimeSpan(date, date+"T00:00:00",date+"T23:59:59"))
      o.addProperty(SKOS.prefLabel,"Order"+(if (r(1)!="N") " of "+clientNameMap(r(1)) else "")+" on "+date,"en")
      orderNameMap.put(r(0),"order"+(if (r(1)!="N") " of "+clientNameMap(r(1)) else "")+" on "+date)
      if (r(4)!="xxxx" && r(4)!="xx") {
        val s = I(ns+"source_"+r(4)+"_"+encode(r(5)),r(4)+" ("+r(5)+")",CIDOC.Document)
        s.addProperty(sourceType,I(ns+"sourceType_"+encode(r(5)),r(5),SourceType))
        o.addProperty(DCTerms.source,s)
      }
      ANE(o,balleNumber,r(6))
      o.addProperty(cash,if (r(7).charAt(0)==0) "false" else "true",XSDDatatype.XSDboolean)
    }
    
    /*CREATE TABLE `orders_agents` (
  `order_code` char(9) NOT NULL,
  `client_code` char(6) NOT NULL,
  `place_code` char(5) DEFAULT NULL,
*/
    
    wr = CSVReader("fbtee/orders_agents.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) {
      val o = R(ns+"order_"+r(0))
      o.addProperty(agent,clientToResourceMap(r(1)))
      if (r(2)!="N") {
        val s = m.createResource
        s.addProperty(RDF.`type`,RDF.Statement)
        s.addProperty(RDF.subject, o)
        s.addProperty(RDF.predicate, agent)
        s.addProperty(RDF.`object`, clientToResourceMap(r(1)))
        s.addProperty(CIDOC.took_place_at, R(ns+"place_"+r(2)))
      }
    }

    /*
CREATE TABLE `orders_sent_via` (
  `order_code` char(9) NOT NULL,
  `client_code` char(6) NOT NULL,
  `place_code` char(5) DEFAULT NULL,
     */
   wr = CSVReader("fbtee/orders_sent_via.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) {
      val o = R(ns+"order_"+r(0))
      o.addProperty(middleman,clientToResourceMap(r(1)))
      if (r(2)!="N") {
        val s = m.createResource
        s.addProperty(RDF.`type`,RDF.Statement)
        s.addProperty(RDF.subject, o)
        s.addProperty(RDF.predicate, middleman)
        s.addProperty(RDF.`object`, clientToResourceMap(r(1)))
        s.addProperty(CIDOC.took_place_at, R(ns+"place_"+r(2)))
        o.addProperty(sentVia,R(ns+"place_"+r(2)))
      }
    }

    /* CREATE TABLE `orders_sent_via_place` (
  `order_code` char(9) NOT NULL,
  `place_code` char(5) NOT NULL,
*/
   wr = CSVReader("fbtee/orders_sent_via_place.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) R(ns+"order_"+r(0)).addProperty(sentVia,R(ns+"place_"+r(1)))
    
    /*CREATE TABLE `transactions` (
  `transaction_code` char(9) NOT NULL,
  `order_code` char(9) NOT NULL,
  `page_or_folio_numbers` varchar(50) NOT NULL,
  `account_heading` varchar(50) DEFAULT NULL,
  `direction_of_transaction` varchar(50) NOT NULL,
  `super_book_code` char(11) NOT NULL,
  `book_code` char(9) DEFAULT NULL,
  `stn_abbreviated_title` varchar(600) NOT NULL,
  `total_number_of_volumes` int(11) DEFAULT NULL,
  `notes` varchar(4000) DEFAULT NULL,*/
   wr = CSVReader("fbtee/transactions.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) {
      val tt = I(sns+"transactionType_"+encode(r(4)),r(4),OWL.Class)
      tt.addProperty(RDFS.subClassOf,Transaction)
      val t = I(ns+"transaction_"+r(0)+"_"+r(1),Map("en"->(r(4)+" of "+r(8)+" volumes of "+r(7)+" as part of "+orderNameMap(r(1)))),tt)
      t.addProperty(DCTerms.isPartOf, R(ns+"order_"+r(1)))
      ANE(t,pageNumber, r(2))
      ANE(t,accountHeading,r(3))
      t.addProperty(CIDOC.had_participant,R(ns+"work_"+r(5)))
      t.addProperty(CIDOC.had_participant,R(ns+"manifestation_"+r(6)))
      t.addProperty(volumes,r(8),XSDDatatype.XSDinteger)
      ANE(t,RDFS.comment,r(9))
    }

    /*CREATE TABLE `transactions_volumes_exchanged` (
  `transaction_code` char(9) NOT NULL,
  `order_code` char(9) NOT NULL,
  `volume_number` int(11) NOT NULL,
  `number_of_copies` int(11) NOT NULL,
     */
   wr = CSVReader("fbtee/transactions.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) {
      val t = R(ns+"transaction_"+r(0)+"_"+r(1))
      if (r(3)=="0")
        t.addProperty(volumeNotExchanged,r(2),XSDDatatype.XSDinteger)
      else {
        t.addProperty(volumeExchanged,r(2),XSDDatatype.XSDinteger)
        val s = m.createResource()
        s.addProperty(RDF.`type`,RDF.Statement)
        s.addProperty(RDF.subject, t)
        s.addProperty(RDF.predicate, volumeExchanged)
        s.addProperty(RDF.`object`, r(2),XSDDatatype.XSDinteger)
        if (r(3)=="N") s.addProperty(numberOfCopies, "unknown") else s.addProperty(numberOfCopies, r(3), XSDDatatype.XSDinteger)
      }
    }
    

    /*CREATE TABLE `parisian_keywords` (
  `parisian_keyword_code` char(5) NOT NULL,
  `parisian_keyword` varchar(250) NOT NULL,
  `ancestor1` char(5) DEFAULT NULL,
  `ancestor2` char(5) DEFAULT NULL,
  `ancestor3` char(5) DEFAULT NULL,*/
    val pkeywordNameCodeMap = new HashMap[String, Resource]
    wr = CSVReader("fbtee/parisian_keywords.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) {
      val pkw = I(ns + "parisian_keyword_" + r(0), Map("fr" -> r(1)), ParisianKeyword)
      if (r(2) != "N") pkw.addProperty(SKOS.broader, R(ns + "parisian_keyword_" + r(2)))
      if (r(3) != "N") pkw.addProperty(SKOS.broader, R(ns + "parisian_keyword_" + r(3)))
      if (r(4) != "N") pkw.addProperty(SKOS.broader, R(ns + "parisian_keyword_" + r(4)))
      pkeywordNameCodeMap.put(r(1), pkw)
    }

    /*CREATE TABLE `parisian_system_keyword_assignments` (
  `super_book_code` char(11) NOT NULL,
  `position` int(11) NOT NULL,
  `parisian_system_keyword` varchar(250) NOT NULL,*/
    wr = CSVReader("fbtee/parisian_keywords.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) R(ns + "work_" + r(0)).addProperty(DCTerms.subject, pkeywordNameCodeMap(r(1)))

    /*CREATE TABLE `people_professions` (
  `person_code` char(6) NOT NULL,
  `profession_code` char(5) NOT NULL,
*/
    wr = CSVReader("fbtee/people_professions.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr)
      R(ns + "person_" + r(0)).addProperty(profession, R(ns + "profession_" + r(1)))

    /* CREATE TABLE `professions` (
  `profession_code` char(5) NOT NULL,
  `profession_type` varchar(50) NOT NULL,
  `translated_profession` varchar(100) NOT NULL DEFAULT 'empty',
  `profession_group` varchar(100) NOT NULL DEFAULT 'empty',
  `economic_sector` varchar(100) NOT NULL DEFAULT 'empty',*/

    wr = CSVReader("fbtee/professions.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) {
      val p = I(ns + "profession_" + r(0), Map("en" -> r(2), "fr" -> r(1)), Profession)
      val pg = I(ns + "profession_group_" + encode(r(3)), Map("en" -> r(3)), Profession)
      p.addProperty(SKOS.broader, pg)
      pg.addProperty(SKOS.broader, I(ns + "economic_sector_" + encode(r(4)), Map("en" -> r(4)), Profession))
    }

    /*CREATE TABLE `super_books` (
  `super_book_code` char(11) NOT NULL,
  `super_book_title` varchar(600) NOT NULL,
  `keywords` varchar(1000) DEFAULT NULL,
  `parisian_keyword` varchar(2000) DEFAULT NULL,
  `illegality` varchar(2000) DEFAULT NULL,*/
    wr = CSVReader("fbtee/super_books.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) {
      val w = I(ns + "work_" + r(0), r(1), FRBR.Work)
      for (kw <- r(2).split(',')) w.addProperty(DCTerms.subject, R(ns + "keyword_" + kw.trim))
      for (kw <- r(3).split(',')) w.addProperty(DCTerms.subject, R(ns + "parisian_keyword_" + kw.trim))
      if (r(4) != "N") w.addProperty(illegality, r(4))
    }

    /*CREATE TABLE `super_books_keywords` (
  `super_book_code` char(11) NOT NULL,
  `keyword_code` char(5) NOT NULL,
     */
    wr = CSVReader("fbtee/super_books_keywords.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) R(ns + "work_" + r(0)).addProperty(DCTerms.subject, R(ns + "keyword_" + r(1)))

    /* CREATE TABLE `tags` (
  `tag_code` varchar(3) CHARACTER SET utf8 NOT NULL,
  `tag` varchar(50) CHARACTER SET utf8 NOT NULL,
  `tag_definition` varchar(1000) CHARACTER SET utf8 NOT NULL*/
    wr = CSVReader("fbtee/tags.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) I(ns + "tag_" + r(0), Map("en" -> r(1)), Tag).addProperty(DCTerms.description, r(2), "en")

    m.setNsPrefix("crm", CIDOC.ns)
    m.setNsPrefix("rdfs", RDFS.uri)
    m.setNsPrefix("skos", SKOS.ns)
    m.setNsPrefix("dct", DCTerms.NS)
    m.setNsPrefix("rdf", RDF.uri)
    m.setNsPrefix("fbtee-schema", sns)
    m.setNsPrefix("fbtee", ns)
    RDFDataMgr.write(new FileOutputStream("fbtee.ttl"), m, RDFFormat.TTL)

  }
}
