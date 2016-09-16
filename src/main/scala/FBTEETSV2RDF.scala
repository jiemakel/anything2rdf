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

object FBTEETSV2RDF extends Anything2RDF {

  val sns = "http://ldf.fi/fbtee-schema#"
  val ns = "http://ldf.fi/fbtee/"

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
  val EditionType = EC("Edition Type")

  val Tag = EC("Tag")
  val tag = EOP("tag")

  val Keyword = EC("Keyword")

  val Profession = EC("Profession")
  val profession = EOP("profession")
  val ParisianKeyword = EC("Parisian Keyword")
  
  val illegality = EDP("illegality")

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
      b.addProperty(SKOS.prefLabel, r(4), "fr")
      for (title <- r(5).split(";")) b.addProperty(SKOS.altLabel, title.trim, "fr")
      if (r(6)!="\0") b.addProperty(SKOS.prefLabel, r(6))
      /* edition status:
    1952 Certain
     263 Probable
    1770 Pseudo
       2 Unreviewed
       */
      
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

    /* CREATE TABLE `clients_addresses` (
  `client_code` char(6) NOT NULL,
  `place_code` char(5) NOT NULL,
  `address` varchar(50) DEFAULT NULL,
     */

    /* CREATE TABLE `clients_correspondence_manuscripts` (
  `client_code` char(6) NOT NULL,
  `position` int(11) NOT NULL,
  `manuscript_numbers` varchar(500) DEFAULT NULL, */

    /* CREATE TABLE `clients_correspondence_places` (
  `client_code` char(6) NOT NULL,
  `place_code` char(5) NOT NULL,
  `from_date` varchar(20) DEFAULT NULL,
     */

    /* CREATE TABLE `clients_people` (
  `client_code` char(6) NOT NULL,
  `person_code` char(6) NOT NULL,
  PRIMARY KEY (`client_code`,`person_code`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;
     */
    val personClientMap = new HashMap[String, Resource]
    wr = CSVReader("fbtee/clients_people.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) personClientMap.put(r(1), R(ns + "client_" + r(0)))

    /* CREATE TABLE `clients_professions` (
  `client_code` char(6) NOT NULL,
  `profession_code` char(5) NOT NULL,
  PRIMARY KEY (`client_code`,`profession_code`)*/
    wr = CSVReader("fbtee/clients_professions.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) R(ns + "client_" + r(0)).addProperty(profession, R(ns + "profession_" + r(1)))

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
    /*CREATE TABLE `orders_agents` (
  `order_code` char(9) NOT NULL,
  `client_code` char(6) NOT NULL,
  `place_code` char(5) DEFAULT NULL,
*/
    /*
CREATE TABLE `orders_sent_via` (
  `order_code` char(9) NOT NULL,
  `client_code` char(6) NOT NULL,
  `place_code` char(5) DEFAULT NULL,
     */

    /* CREATE TABLE `orders_sent_via_place` (
  `order_code` char(9) NOT NULL,
  `place_code` char(5) NOT NULL,
*/

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
    wr = CSVReader("fbtee/people.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) {
      val p = personClientMap.getOrElseUpdate(r(0), R(ns + "person_" + r(0)))
    }

    /*CREATE TABLE `people_professions` (
  `person_code` char(6) NOT NULL,
  `profession_code` char(5) NOT NULL,
*/
    wr = CSVReader("fbtee/people_professions.txt")(CSVReaderSettings.Standard.copy(separator = '\t', quotechar = '|'))
    for (r <- wr) personClientMap.getOrElseUpdate(r(0), R(ns + "person_" + r(0))).addProperty(profession, R(ns + "profession_" + r(1)))

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
      for (kw <- r(2).split(',')) w.addProperty(DCTerms.subject, R(ns + "keyword_" + kw))
      for (kw <- r(3).split(',')) w.addProperty(DCTerms.subject, R(ns + "parisian_keyword_" + kw))
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

    /*CREATE TABLE `transactions_volumes_exchanged` (
  `transaction_code` char(9) NOT NULL,
  `order_code` char(9) NOT NULL,
  `volume_number` int(11) NOT NULL,
  `number_of_copies` int(11) NOT NULL,
     */
    m.setNsPrefix("crm", CIDOC.ns)
    m.setNsPrefix("rdfs", RDFS.uri)
    m.setNsPrefix("skos", SKOS.ns)
    m.setNsPrefix("dct", DCTerms.NS)
    m.setNsPrefix("fbtee-schema", sns)
    m.setNsPrefix("fbtee", ns)
    RDFDataMgr.write(new FileOutputStream("fbtee.ttl"), m, RDFFormat.TTL)

  }
}
