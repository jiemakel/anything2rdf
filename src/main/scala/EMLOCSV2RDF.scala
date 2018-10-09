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
import org.apache.jena.vocabulary.DCTerms
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import scala.util.control.Breaks._
import org.apache.jena.rdf.model.Property
import scala.util.Try
import org.apache.jena.iri.IRIFactory

object EMLOCSV2RDF extends Anything2RDF {

  val sns = "http://emlo.bodleian.ox.ac.uk/schema#"
  val ns = "http://emlo.bodleian.ox.ac.uk/id/"
  val credits = EOP("Credits")

  val Letter = EC("Letter")
  Letter.addProperty(RDFS.subClassOf,CIDOC.Linguistic_Object)

  val City = EC("City")
  City.addProperty(RDFS.subClassOf, CIDOC.Place)
  val Country = EC("Country")
  Country.addProperty(RDFS.subClassOf, CIDOC.Place)

  val Source = EC("Source")
  val Calendar = EC("Calendar")
  val Keyword = EC("Keyword")
  val Language = EC("Language")

  val pdate = EOP("possible date")
  val adate = EOP("approximate date")
  val idate = EOP("inferred date")

  val bdate = EOP("birth date")
  val pbdate = EOP("possible birth date")
  val abdate = EOP("approximate birth date")
  val ibdate = EOP("inferred birth date")
  val ddate = EOP("death date")
  val pddate = EOP("possible death date")
  val addate = EOP("approximate death date")
  val iddate = EOP("inferred death date")

  val authors_as_marked = EDP("authors as marked")
  val authors_uncertain = EOP("uncertain author")
  val authors_inferred = EOP("inferred author")
  val addressees_as_marked = EDP("addressees as marked")
  val addressees_uncertain = EOP("uncertain addressee")
  val addressees_inferred = EOP("inferred addressee")
  val origin_as_marked = EDP("origin as marked")
  val origin_inferred = EDP("inferred origin")
  val origin_uncertain = EDP("uncertain origin")
  val destination_as_marked = EDP("destination as marked")
  val destination_inferred = EDP("inferred destination")
  val destination_uncertain = EDP("uncertain destination")
  val incipit = EDP("incipit")
  val ps = EDP("ps")
  val is_translation = EDP("is translation")
  val excipit = EDP("excipit")
  val source = EOP("source")
  val original_calendar = EOP("original calendar")

  val iperson_id = EDP("iperson_id")
  val place_id = EDP("place_id")
  val iwork_id = EDP("iwork_id")

  val id_number_or_shelfmark = EDP("id number or shelfmark")
  val printed_edition_details = EDP("printed edition details")
  val paper_size = EDP("paper size")
  val paper_type_or_watermark = EDP("paper type or watermark")
  val number_of_pages_of_document = EDP("number of pages of document")
  val number_of_pages_of_text = EDP("number of pages of text")
  val seal = EDP("seal")
  val postage_marks = EDP("postage marks")
  val endorsements = EDP("endorsements")
  val non_letter_enclosures = EDP("non letter enclosures")
  val address = EDP("address")

  class Metadata(
    val author_property : Property,
    val addressee_property : Property,
    val origin_property : Property,
    val destination_property : Property
  )


  val unCCRegex = "(?<=[A-Z])(?=[A-Z][a-z])|(?<=[^A-Z])(?=[A-Z])|(?<=[A-Za-z])(?=[^A-Za-z])".r
  def unCamelCase(str : String) : String = {
    unCCRegex.replaceAllIn(str, " ")
  }

  val uuidMap = new HashMap[String,String]
  def putUUID(id : String, uuid : String) = {
    uuidMap.put(id,uuid)
  }

  def getUUIDURI(id : String) : String = {
    ns+uuidMap.getOrElse(id,id)
  }

  def iriFix(iri: String): String = {
    val ret = new StringBuilder()
    for (char <- iri) char match {
      case ' ' => ret.append("%20")
      case '<' => ret.append("%3C")
      case '>' => ret.append("%3E")
      case '"' => ret.append("%22")
      case '{' => ret.append("%7B")
      case '}' => ret.append("%7D")
      case '[' => ret.append("%5B")
      case ']' => ret.append("%5D")
      case '|' => ret.append("%7C")
      case '\\' => ret.append("%5C")
      case '^' => ret.append("%5E")
      case '`' => ret.append("%60")
      case _ => ret.append(char)
    }
    ret.toString()
  }

  def main(args: Array[String]): Unit = {
    var wr : CSVReader = null
    var headers: Array[String] = null
    var h: Map[String,Int] = null
    wr = CSVReader("image.csv")
    headers = wr.next
    h = headers.zipWithIndex.toMap
    val imageMap = new HashMap[String,Resource]
    breakable { for (w <- wr) {
      val i = m.createResource(iriFix(w(h("image_filename"))))
      i.addProperty(RDF.`type`,CIDOC.Image)
      if (!w(h("credits")).trim.isEmpty) i.addProperty(DCTerms.license,w(h("credits")))
      imageMap.put(w(h("image_id")),i)

    }}
    val resourceMap = new HashMap[String,Resource]
//csv2rdf List(image_id, image_filename, creation_timestamp, creation_user, change_timestamp, change_user, thumbnail, display_order, credits)
    wr = CSVReader("resource.csv")
    headers = wr.next
    h = headers.zipWithIndex.toMap
    breakable { for (w <- wr) {
      val iri = if (!w(h("resource_url")).trim.isEmpty) {
        val iri2 = iriFix(w(h("resource_url")).trim)
        val violations = IRIFactory.iriImplementation().create(iri2).violations(false)
        if (violations.hasNext) {
          var violationMessage=""
          while (violations.hasNext) violationMessage+=violations.next.getShortMessage+", "
          logger.warn("Bad URL: "+w(h("resource_url"))+"("+violationMessage+")")
          ns+w(h("resource_id"))
        } else iri2
      } else ns+w(h("resource_id"))
      val res = I(iri,w(h("resource_name")),CIDOC.Information_Object)
      resourceMap.put(w(h("resource_id")),res)
      if (!w(h("resource_details")).trim.isEmpty) res.addProperty(DCTerms.license,w(h("resource_details")))
    }}
    val manifestationWorkMap = new HashMap[String,String]
    wr = CSVReader("relationship.csv")
    headers = wr.next
    h = headers.zipWithIndex.toMap
    val ind = h("relationship_type")
    val liv = h("left_id_value")
    val riv = h("right_id_value")
    for (w <- wr)
        if (w(ind)=="cofk_union_relationship_type-is_manifestation_of") manifestationWorkMap.put(w(liv),w(riv))
    wr = CSVReader("work.csv")
    headers = wr.next
    h = headers.zipWithIndex.toMap
    val workNameMap = new HashMap[String,String]
    val workMetadataMap = new HashMap[String,Metadata]
    breakable { for (w <- wr) {
      workNameMap.put(w(h("work_id")),w(h("description")))
      putUUID(w(h("work_id")),w(h("uuid")))
      val wo = I(getUUIDURI(w(h("work_id"))),Map("en"->w(h("description"))),Letter)
      if (!w(h("iwork_id")).trim.isEmpty) wo.addProperty(iwork_id,w(h("iwork_id")))
      if (!w(h("original_calendar")).trim.isEmpty) wo.addProperty(original_calendar,I(ns+"calendar_"+encode(w(h("original_calendar"))),Map("en"->w(h("original_calendar"))),Calendar))
      if (!w(h("original_catalogue")).trim.isEmpty) wo.addProperty(source,I(ns+"source_"+encode(w(h("original_catalogue"))),w(h("original_catalogue")),Source))
      if (!w(h("abstract")).trim.isEmpty) wo.addProperty(DCTerms.description,w(h("abstract")).trim,"en")
      if (!w(h("editors_notes")).trim.isEmpty) wo.addProperty(RDFS.comment,w(h("editors_notes")),"en")
      if (!w(h("incipit")).trim.isEmpty) wo.addProperty(incipit,w(h("incipit")))
      if (!w(h("explicit")).trim.isEmpty) wo.addProperty(excipit,w(h("explicit")))
      if (!w(h("date_of_work_as_marked")).trim.isEmpty) wo.addProperty(DCTerms.date,w(h("date_of_work_as_marked")))
      if (!w(h("ps")).trim.isEmpty) wo.addProperty(ps,w(h("ps")))
      w(h("keywords")).split(Array('\n',';','.')).map(_.trim).filter(!_.isEmpty).foreach(k => wo.addProperty(DCTerms.subject,I(ns+"keyword_"+encode(k),Map("en"->k),Keyword)));
      if (w(h("work_is_translation"))=='1') wo.addLiteral(is_translation,true)
      if (w(h("authors_as_marked"))=='1') wo.addProperty(authors_as_marked,w(h("authors_as_marked")))
      val aprop = if (w(h("authors_uncertain"))=='1') authors_uncertain
      else if (w(h("authors_inferred"))=='1') authors_inferred
      else m.createProperty(sns+"cofk_union_relationship_type-created")
      val rprop = if (w(h("addressees_uncertain"))=='1') addressees_uncertain
      else if (w(h("addressees_inferred"))=='1') addressees_inferred
      else m.createProperty(sns+"cofk_union_relationship_type-was_addressed_to")
      val oprop = if (w(h("origin_uncertain"))=='1') origin_uncertain
      else if (w(h("origin_inferred"))=='1') origin_inferred
      else m.createProperty(sns+"cofk_union_relationship_type-was_sent_from")
      val dprop = if (w(h("destination_uncertain"))=='1') destination_uncertain
      else if (w(h("destination_inferred"))=='1') destination_inferred
      else m.createProperty(sns+"cofk_union_relationship_type-was_sent_to")
      val md = new Metadata(aprop,rprop,oprop,dprop)
      workMetadataMap.put(w(h("work_id")),md)
      w(h("language_of_work")).split(',').map(_.trim).filter(!_.isEmpty).foreach(k => wo.addProperty(DCTerms.language,I(ns+"language_"+encode(k),Map("en"->k),Language)));
      if (!w(h("date_of_work_std_year")).trim.isEmpty) {
        val date = if (w(h("date_of_work_std_is_range"))=='1') {
          val (bdateTime,edateTime) = makeDateTime(w(h("date_of_work_std_year")),w(h("date_of_work_std_month")),w(h("date_of_work_std_day")))
          val (bdateTime2,edateTime2) = makeDateTime(w(h("date_of_work2_std_year")),w(h("date_of_work2_std_month")),w(h("date_of_work2_std_day")))
          makeTimeSpan(s"${bdateTime.substring(0,10)} - ${edateTime2.substring(0,10)}", Some(bdateTime), Some(edateTime), Some(bdateTime2), Some(edateTime2))
        } else {
          val (bdateTime,edateTime) = makeDateTime(w(h("date_of_work_std_year")),w(h("date_of_work_std_month")),w(h("date_of_work_std_day")))
          makeTimeSpan(s"${bdateTime.substring(0,10)}",Some(bdateTime),Some(edateTime),Some(bdateTime),Some(edateTime))
        }
        if (w(h("date_of_work_approx"))=="1") wo.addProperty(adate,date)
        else if (w(h("date_of_work_uncertain"))=="1") wo.addProperty(pdate,date)
        else if (w(h("date_of_work_inferred"))=="1") wo.addProperty(idate,date)
        else wo.addProperty(CIDOC.has_timeSpan,date)
      }

    }}
/*
 * csv2rdf Map(addressees_uncertain -> 0, date_of_work_std -> 1779-10-03, date_of_work_inferred -> 0,
 * date_of_work_std_gregorian -> 1779-10-03, date_of_work_std_year -> 1779,
 * destination_as_marked -> , editors_notes -> , date_of_work2_std_day -> , date_of_work_approx -> 0,
 * incipit -> , description -> 3 Oct 1779: Badcock, Samuel, 1747-1788 to Griffiths, Ralph, 1720-1803,
 * original_catalogue -> Bodleian card catalogue,
 * authors_inferred -> 0,
 * date_of_work_uncertain -> 0,
 * date_of_work_as_marked -> Oct. 3 1779,
 * addressees_inferred -> 0, destination_inferred -> 0, edit_status ->
 * date_of_work_std_is_range -> 0, ps -> ,
 * creation_timestamp -> 2010-02-24 00:00:00, date_of_work_std_month -> 10, work_is_translation -> 0,
 * origin_as_marked -> , keywords -> ,
 * language_of_work -> English,
 * date_of_work2_std_year -> ,
 * original_calendar -> Unknown,
 * addressees_as_marked -> Griffiths, R.,
 * explicit -> ,
 * origin_inferred -> 0, authors_uncertain -> 0,
 * date_of_work_std_day -> 3,
 * destination_uncertain -> 0, origin_uncertain -> 0,
 * date_of_work2_std_month -> , authors_as_marked -> Badcock, S., 1747-1788)
 */

//csv2rdf List(work_id, description, date_of_work_as_marked, original_calendar, date_of_work_std, date_of_work_std_gregorian, date_of_work_std_year, date_of_work_std_month, date_of_work_std_day, date_of_work2_std_year, date_of_work2_std_month, date_of_work2_std_day, date_of_work_std_is_range, date_of_work_inferred, date_of_work_uncertain, date_of_work_approx, authors_as_marked, addressees_as_marked, authors_inferred, authors_uncertain, addressees_inferred, addressees_uncertain, destination_as_marked, origin_as_marked, destination_inferred, destination_uncertain, origin_inferred, origin_uncertain, abstract, keywords, language_of_work, work_is_translation, incipit, explicit, ps, original_catalogue, accession_code, work_to_be_deleted, iwork_id, editors_notes, edit_status, relevant_to_cofk, creation_timestamp, creation_user, change_timestamp, change_user)
    wr = CSVReader("manifestation.csv")
    headers = wr.next
    h = headers.zipWithIndex.toMap
    val manifestationMap = new HashMap[String,Resource]
    breakable { for (w <- wr) if (!manifestationWorkMap.contains(w(h("manifestation_id")))) logger.warn(s"Manifestation ${w(h("manifestation_id"))} is a manifestation of no work!") else {
      val t = EC(w(h("manifestation_type")))
      t.addProperty(RDFS.subClassOf,CIDOC.Information_Carrier)
      val ma = if (w(h("manifestation_type"))=="Letter") {
        val tmp = m.createResource(getUUIDURI(manifestationWorkMap(w(h("manifestation_id")))))
        manifestationMap.put(w(h("manifestation_id")),tmp)
        tmp
      } else {
        putUUID(w(h("manifestation_id")),w(h("uuid")))
        I(getUUIDURI(w(h("manifestation_id"))),w(h("manifestation_type"))+" of "+manifestationWorkMap.get(w(h("manifestation_id"))).flatMap(workNameMap.get(_)).getOrElse("?"),t)
      }
      if (!w(h("id_number_or_shelfmark")).trim.isEmpty) ma.addProperty(id_number_or_shelfmark,w(h("id_number_or_shelfmark")))
      if (!w(h("printed_edition_details")).trim.isEmpty) ma.addProperty(printed_edition_details,w(h("printed_edition_details")))
      if (!w(h("paper_size")).trim.isEmpty) ma.addProperty(paper_size,w(h("paper_size")))
      if (!w(h("paper_type_or_watermark")).trim.isEmpty) ma.addProperty(paper_type_or_watermark,w(h("paper_type_or_watermark")))
      if (!w(h("number_of_pages_of_document")).trim.isEmpty) ma.addProperty(number_of_pages_of_document,w(h("number_of_pages_of_document")))
      if (!w(h("number_of_pages_of_text")).trim.isEmpty) ma.addProperty(number_of_pages_of_text,w(h("number_of_pages_of_text")))
      if (!w(h("seal")).trim.isEmpty) ma.addProperty(seal,w(h("seal")))
      if (!w(h("postage_marks")).trim.isEmpty) ma.addProperty(postage_marks,w(h("postage_marks")))
      if (!w(h("endorsements")).trim.isEmpty) ma.addProperty(endorsements,w(h("endorsements")))
      if (!w(h("non_letter_enclosures")).trim.isEmpty) ma.addProperty(non_letter_enclosures,w(h("non_letter_enclosures")))
      if (!w(h("address")).trim.isEmpty) ma.addProperty(address,w(h("address")))
      if (!w(h("manifestation_incipit")).trim.isEmpty) ma.addProperty(incipit,w(h("manifestation_incipit")))
      if (!w(h("manifestation_excipit")).trim.isEmpty) ma.addProperty(excipit,w(h("manifestation_excipit")))
      if (!w(h("manifestation_ps")).trim.isEmpty) ma.addProperty(ps,w(h("manifestation_ps")))
      if (w(h("manifestation_is_translation"))=='1') ma.addLiteral(is_translation,true)
      w(h("language_of_manifestation")).split(',').map(_.trim).filter(!_.isEmpty).foreach(k => ma.addProperty(DCTerms.language,I(ns+"language_"+encode(k),Map("en"->k),Language)));
      if (!w(h("manifestation_creation_calendar")).trim.isEmpty) ma.addProperty(original_calendar,I(ns+"calendar_"+encode(w(h("manifestation_creation_calendar"))),Map("en"->w(h("manifestation_creation_calendar"))),Calendar))
      if (!w(h("manifestation_creation_date_year")).trim.isEmpty) {
        val (bdateTime,edateTime) = makeDateTime(w(h("manifestation_creation_date_year")),w(h("manifestation_creation_date_month")),w(h("manifestation_creation_date_day")))
        val date = makeTimeSpan(s"${bdateTime.substring(0,10)}-${edateTime.substring(0,10)}",Some(bdateTime),Some(edateTime),Some(bdateTime),Some(edateTime))
        if (w(h("manifestation_creation_date_approx"))=="1") ma.addProperty(adate,date)
        else if (w(h("manifestation_creation_date_uncertain"))=="1") ma.addProperty(pdate,date)
        else if (w(h("manifestation_creation_date_inferred"))=="1") ma.addProperty(idate,date)
        else ma.addProperty(CIDOC.has_timeSpan,date)
      }

    }}
//csv2rdf List(manifestation_id, manifestation_type, id_number_or_shelfmark, printed_edition_details, paper_size, paper_type_or_watermark,
//number_of_pages_of_document, number_of_pages_of_text, seal, postage_marks, endorsements, non_letter_enclosures,
//manifestation_creation_calendar, manifestation_creation_date, manifestation_creation_date_gregorian, manifestation_creation_date_year,
//manifestation_creation_date_month, manifestation_creation_date_day, manifestation_creation_date_inferred,
//manifestation_creation_date_uncertain, manifestation_creation_date_approx, manifestation_is_translation, language_of_manifestation,
//address, manifestation_incipit, manifestation_excipit, manifestation_ps)
    wr = CSVReader("location.csv")
    headers = wr.next
    h = headers.zipWithIndex.toMap
    val locationMap = new HashMap[String,Resource]()
    val locationFNMap = new HashMap[String,Resource]()
    breakable { for (w <- wr) {
      putUUID(w(h("location_id")),w(h("uuid")))
      val l = m.createResource(getUUIDURI(w(h("location_id"))))
      l.addProperty(place_id,w(h("location_id")).substring(20))
      l.addProperty(RDF.`type`,CIDOC.Place)
      l.addProperty(RDFS.label,w(h("location_name")))
      w(h("location_synonyms")).split('\n').map(_.trim).filter(!_.isEmpty).foreach{n =>
        l.addProperty(SKOS.altLabel,n)
        locationMap.put(n,l)
       }
      locationMap.put(w(h("location_name")).split(',')(0).trim,l)
      locationMap.put(w(h("location_name")),l)
      locationFNMap.put(w(h("location_name")),l)
      if (!w(h("latitude")).trim.isEmpty) l.addProperty(WGS84.lat,w(h("latitude")))
      if (!w(h("longitude")).trim.isEmpty) l.addProperty(WGS84.long,w(h("longitude")))

    }}
    locationFNMap.foreach{
      case (fn,ol) => fn.split(',').map(_.trim).foldRight(null.asInstanceOf[Resource])((n,sl) => {
        val l = locationMap.getOrElseUpdate(n,I(ns+"location_"+encode(n),n,CIDOC.Place))
        l.addProperty(SKOS.prefLabel,n)
        if (sl!=null) l.addProperty(CIDOC.place_falls_within,sl)
        l
      })
    }
//csv2rdf List(location_id, location_name, latitude, longitude, creation_timestamp, creation_user, change_timestamp, change_user, location_synonyms, sent_count, recd_count, mentioned_count)*/
    wr = CSVReader("institution.csv")
    headers = wr.next
    h = headers.zipWithIndex.toMap
    breakable { for (w <- wr) {
      putUUID(w(h("institution_id")),w(h("uuid")))
      val i = I(getUUIDURI(w(h("institution_id"))),w(h("institution_name")),CIDOC.Legal_Body)
      w(h("institution_synonyms")).split('\n').map(_.trim).filter(!_.isEmpty).foreach(n => i.addProperty(SKOS.altLabel,n))
      val city = if (!w(h("institution_city")).trim.isEmpty) {
        val city = locationMap.getOrElse(w(h("institution_city")), {
            val possible = w(h("institution_city_synonyms")).split('\n').map(_.trim).filter(!_.isEmpty).flatMap(locationMap.get(_))
            if (possible.isEmpty) {
              val ret = I(ns+"location_"+encode(w(h("institution_city"))),w(h("institution_city")),City)
              w(h("institution_city_synonyms")).split('\n').map(_.trim).filter(!_.isEmpty).foreach(n => locationMap.put(n,ret))
              ret
            }
            else possible(0)
        })
        city.addProperty(RDF.`type`,City)
        Some(city)
      } else None
      val country = if (!w(h("institution_country")).trim.isEmpty) {
        val country = locationMap.getOrElse(w(h("institution_country")), {
            val possible = w(h("institution_country_synonyms")).split('\n').map(_.trim).filter(!_.isEmpty).flatMap(locationMap.get(_))
            if (possible.isEmpty) {
              val ret = I(ns+"location_"+encode(w(h("institution_country"))),w(h("institution_country")),Country)
              w(h("institution_country_synonyms")).split('\n').map(_.trim).filter(!_.isEmpty).foreach(n => locationMap.put(n,ret))
              ret
            }
            else possible(0)

        })
        country.addProperty(RDF.`type`,Country)
        city.foreach(c => c.addProperty(CIDOC.place_falls_within,country))
        Some(country)
      } else None
      city.orElse(country).foreach(p => i.addProperty(ORG.hasPrimarySite,p))

    }}
//csv2rdf List(institution_id, institution_name, institution_synonyms, institution_city, institution_city_synonyms, institution_country, institution_country_synonyms, creation_timestamp, creation_user, change_timestamp, change_user, document_count)
    wr = CSVReader("person.csv")
    headers = wr.next
    h = headers.zipWithIndex.toMap
    val personIdMap = new HashMap[String,Resource]
    val personNameMap = new HashMap[String,String]
    breakable { for (w <- wr) {
      putUUID(w(h("person_id")),w(h("uuid")))
      val p = I(getUUIDURI(w(h("person_id"))),w(h("foaf_name")),if (!w(h("is_organisation")).trim.isEmpty) CIDOC.Group else CIDOC.Person)
      personIdMap.put(w(h("iperson_id")),p)
      personNameMap.put(w(h("iperson_id")),w(h("foaf_name")))
      if (!w(h("gender")).trim.isEmpty) {
        (w(h("gender"))) match {
          case "M" => p.addProperty(FOAF.gender,SDMXCode.sexMale)
          case "F" => p.addProperty(FOAF.gender,SDMXCode.sexFemale)
          case _ => logger.warn("Unknown gender "+w(h("gender")))
        }
      }
      p.addProperty(iperson_id,w(h("iperson_id")))
      if (!w(h("further_reading")).trim.isEmpty) p.addProperty(RDFS.comment,w(h("further_reading")))
      p.addProperty(FOAF.name,w(h("foaf_name")))
      if (!w(h("skos_altlabel")).trim.isEmpty) p.addProperty(SKOS.altLabel,w(h("skos_altlabel")))
      w(h("skos_hiddenlabel")).split(Array('/',';')).map(_.trim).filter(!_.isEmpty).foreach(p.addProperty(SKOS.hiddenLabel,_))
      w(h("person_aliases")).split(Array('\n',';')).map(_.trim).filter(!_.isEmpty).foreach(p.addProperty(SKOS.altLabel,_))
      if (!w(h("date_of_birth_year")).trim.isEmpty) {
        val (bdateTime,edateTime) = makeDateTime(w(h("date_of_birth_year")),w(h("date_of_birth_month")),w(h("date_of_birth_day")))
        val date = makeTimeSpan(s"${bdateTime.substring(0,10)}-${edateTime.substring(0,10)}",Some(bdateTime),Some(edateTime),Some(bdateTime),Some(edateTime))
        if (w(h("date_of_birth_approx"))=="1") p.addProperty(abdate,date)
        else if (w(h("date_of_birth_uncertain"))=="1") p.addProperty(pbdate,date)
        else if (w(h("date_of_birth_inferred"))=="1") p.addProperty(ibdate,date)
        else p.addProperty(bdate,date)
      }
      if (!w(h("date_of_death_year")).trim.isEmpty) {
        val (bdateTime,edateTime) = makeDateTime(w(h("date_of_death_year")),w(h("date_of_death_month")),w(h("date_of_death_day")))
        val date = makeTimeSpan(s"${bdateTime.substring(0,10)}-${edateTime.substring(0,10)}",Some(bdateTime),Some(edateTime),Some(bdateTime),Some(edateTime))
        if (w(h("date_of_death_approx"))=="1") p.addProperty(addate,date)
        else if (w(h("date_of_death_uncertain"))=="1") p.addProperty(pddate,date)
        else if (w(h("date_of_death_inferred"))=="1") p.addProperty(iddate,date)
        else p.addProperty(ddate,date)
      }

    }}
//csv2rdf List(person_id, foaf_name, skos_altlabel, skos_hiddenlabel, person_aliases, date_of_birth_year, date_of_birth_month, date_of_birth_day, date_of_birth, date_of_birth_inferred, date_of_birth_uncertain, date_of_birth_approx, date_of_death_year, date_of_death_month, date_of_death_day, date_of_death, date_of_death_inferred, date_of_death_uncertain, date_of_death_approx, gender, is_organisation, iperson_id, creation_timestamp, creation_user, change_timestamp, change_user, further_reading, sent_count, recd_count, mentioned_count)
    wr = CSVReader("relationship.csv")
    headers = wr.next
    h = headers.zipWithIndex.toMap
    breakable { for (w <- wr) {
        w(ind) match {
          case "cofk_union_relationship_type-created" => m.add(m.createResource(getUUIDURI(w(liv))),workMetadataMap.get(w(liv)).map(_.author_property).getOrElse(m.createProperty(sns+"cofk_union_relationship_type-created")),m.createResource(getUUIDURI(w(riv))))
          case "cofk_union_relationship_type-was_sent_from" => m.add(m.createResource(getUUIDURI(w(liv))),workMetadataMap.get(w(liv)).map(_.origin_property).getOrElse(m.createProperty(sns+"cofk_union_relationship_type-was_sent_from")),m.createResource(getUUIDURI(w(riv))))
          case "cofk_union_relationship_type-was_sent_to" => m.add(m.createResource(getUUIDURI(w(liv))),workMetadataMap.get(w(liv)).map(_.destination_property).getOrElse(m.createProperty(sns+"cofk_union_relationship_type-was_sent_to")),m.createResource(getUUIDURI(w(riv))))
          case "cofk_union_relationship_type-was_addressed_to" => m.add(m.createResource(getUUIDURI(w(liv))),workMetadataMap.get(w(liv)).map(_.addressee_property).getOrElse(m.createProperty(sns+"cofk_union_relationship_type-was_addressed_to")),m.createResource(getUUIDURI(w(riv))))
          case "cofk_union_relationship_type-is_manifestation_of" => if (manifestationMap.get(w(liv)).isEmpty) m.add(m.createResource(getUUIDURI(w(liv))),m.createProperty(sns+w(ind)),m.createResource(getUUIDURI(w(riv))))
          case _ =>
            val liri = manifestationMap.getOrElse(w(liv),imageMap.getOrElse(w(liv), resourceMap.getOrElse(w(liv),m.createResource(getUUIDURI(w(liv))))))
            val riri = manifestationMap.getOrElse(w(riv),imageMap.getOrElse(w(riv), resourceMap.getOrElse(w(riv),m.createResource(getUUIDURI(w(riv))))))
            m.add(liri,m.createProperty(sns+w(ind)),riri)
        }

    }}
//relationship_id,left_table_name,left_id_value,relationship_type,right_table_name,right_id_value,relationship_valid_from,relationship_valid_till,creation_timestamp,creation_user,change_timestamp,change_user
    wr = CSVReader("relationship_type.csv")
    headers = wr.next
    h = headers.zipWithIndex.toMap
    breakable { for (w <- wr) {
      val p = I(sns+w(h("relationship_code")),Map("en"->w(h("desc_left_to_right"))),OWL.ObjectProperty)
      val ip = I(sns+w(h("relationship_code"))+"_inverse",Map("en"->w(h("desc_right_to_left"))),OWL.ObjectProperty)
      p.addProperty(OWL.inverseOf,ip)
      ip.addProperty(OWL.inverseOf,p)

    }}
//csv2rdf List(relationship_id, left_table_name, left_id_value, relationship_type, right_table_name, right_id_value, relationship_valid_from, relationship_valid_till, creation_timestamp, creation_user, change_timestamp, change_user)
    wr = CSVReader("pro_activity.csv")
    headers = wr.next
    h = headers.zipWithIndex.toMap
    val activityTypeNameMap = new HashMap[String,String]
    for (w <- wr) {
      val e = R(ns+"activity_"+w(h("id")))
      ANE(e,RDFS.comment,w(h("additional_notes")),"en")
      val pname = w(h("notes_used")).trim
      if (!pname.isEmpty)
        e.addProperty(DCTerms.source,I(ns+"person_"+encode(pname),pname,CIDOC.Person))
      val dprop = if (w(h("date_to_uncertainty"))=="Uncertain" || w(h("date_from_uncertainty"))=="Uncertain") pdate
      else if (w(h("date_to_uncertainty"))=="Inferred" || w(h("date_from_uncertainty"))=="Inferred") idate
      else if (w(h("date_to_uncertainty"))=="Approximate" || w(h("date_from_uncertainty"))=="Approximate") adate
      else CIDOC.has_timeSpan
      val (name,bob,eob,boe,eoe) = if (w(h("date_from_year")).trim.isEmpty && (!w(h("date_from_month")).trim.isEmpty || !w(h("date_from_day")).trim.isEmpty)) {
        logger.warn(s"borked from-date: ${makeDateString(w(h("date_from_year")),w(h("date_from_month")),w(h("date_from_day")))}")
        ("",None,None,None,None)
      } else if (w(h("date_to_year")).trim.isEmpty && (!w(h("date_to_month")).trim.isEmpty || !w(h("date_to_day")).trim.isEmpty)) {
        logger.warn(s"borked to-date: ${makeDateString(w(h("date_to_year")),w(h("date_to_month")),w(h("date_to_day")))}")
        ("",None,None,None,None)
      } else w(h("date_type")) match {
        case "Before" =>
          val (boe,eoe) = if (!w(h("date_to_year")).trim.isEmpty) makeDateTime(w(h("date_to_year")),w(h("date_to_month")), w(h("date_to_day"))) else makeDateTime(w(h("date_from_year")),w(h("date_from_month")),w(h("date_from_day")))
          ("Before "+ (if (!w(h("date_to_year")).trim.isEmpty) makeDateString(w(h("date_to_year")),w(h("date_to_month")), w(h("date_to_day"))) else makeDateString(w(h("date_from_year")),w(h("date_from_month")),w(h("date_from_day")))),None,None,Some(boe),Some(eoe))
        case "After" =>
          val (bob,eob) = if (!w(h("date_from_year")).trim.isEmpty) makeDateTime(w(h("date_from_year")),w(h("date_from_month")), w(h("date_from_day"))) else makeDateTime(w(h("date_to_year")),w(h("date_to_month")),w(h("date_to_day")))
          ("After "+( if (!w(h("date_from_year")).trim.isEmpty) makeDateString(w(h("date_from_year")),w(h("date_from_month")),w(h("date_from_day"))) else makeDateString(w(h("date_to_year")),w(h("date_to_month")), w(h("date_to_day")))),Some(bob),Some(eob),None,None)
        case "Between" =>
          val (bob,_) = if (!w(h("date_from_year")).trim.isEmpty) makeDateTime(w(h("date_from_year")),w(h("date_from_month")),w(h("date_from_day"))) else makeDateTime(w(h("date_to_year")),w(h("date_to_month")), w(h("date_to_day")))
          val (_,eoe) = if (!w(h("date_to_year")).trim.isEmpty) makeDateTime(w(h("date_to_year")),w(h("date_to_month")), w(h("date_to_day"))) else makeDateTime(w(h("date_from_year")),w(h("date_from_month")),w(h("date_from_day")))
          ("Between " + makeDateString(w(h("date_from_year")),w(h("date_from_month")),w(h("date_from_day")))+ " and "+makeDateString(w(h("date_to_year")),w(h("date_to_month")), w(h("date_to_day"))),Some(bob),Some(eoe),Some(bob),Some(eoe))
        case "Duration" =>
          val (bob,eob) = if (!w(h("date_from_year")).trim.isEmpty) makeDateTime(w(h("date_from_year")),w(h("date_from_month")),w(h("date_from_day"))) else makeDateTime(w(h("date_to_year")),w(h("date_to_month")), w(h("date_to_day")))
          val (boe,eoe) = if (!w(h("date_to_year")).trim.isEmpty) makeDateTime(w(h("date_to_year")),w(h("date_to_month")), w(h("date_to_day"))) else makeDateTime(w(h("date_from_year")),w(h("date_from_month")),w(h("date_from_day")))
          (makeDateString(w(h("date_from_year")),w(h("date_from_month")),w(h("date_from_day")))+'-'+makeDateString(w(h("date_to_year")),w(h("date_to_month")), w(h("date_to_day"))),Some(bob),Some(eob),Some(boe),Some(eoe))
        case "" =>
          if (w(h("date_from_year")).isEmpty) {
            if (!w(h("date_to_year")).trim.isEmpty || !w(h("date_to_month")).trim.isEmpty || !w(h("date_to_day")).trim.isEmpty) logger.warn(s"extraneous to-date: ${makeDateString(w(h("date_to_year")),w(h("date_to_month")),w(h("date_to_day")))}")
            ("",None,None,None,None)
          } else {
            val tmp = Try(if (!w(h("date_from_year")).isEmpty) makeDateTime(w(h("date_from_year")),w(h("date_from_month")),w(h("date_from_day")))
            else makeDateTime(w(h("date_to_year")),w(h("date_to_month")), w(h("date_to_day"))))
            if (tmp.isFailure) {
              logger.warn(s"borked from-date: ${makeDateString(w(h("date_from_year")),w(h("date_from_month")),w(h("date_from_day")))}")
              ("",None,None,None,None)
            } else {
              val (bob,eoe) = tmp.get
              (makeDateString(w(h("date_from_year")),w(h("date_from_month")),w(h("date_from_day"))),Some(bob),Some(eoe),Some(bob),Some(eoe))
            }
          }
      }
      if (!name.isEmpty) {
        val ts = makeTimeSpan(name, bob, eob, boe, eoe)
        e.addProperty(dprop,ts)
      }
      ANE(e,DCTerms.description,w(h("activity_name")))
      ANE(e,RDFS.comment,w(h("activity_description")))
      val atype = w(h("activity_type_id"))
      val aname = unCamelCase(atype)
      activityTypeNameMap.put(w(h("id")),aname)
      val atypeC = I(sns+"activityType_"+encode(atype),Map("en"->aname),OWL.Class)
      atypeC.addProperty(RDFS.subClassOf,CIDOC.Activity)
      e.addProperty(RDF.`type`,atypeC)
    }
//id,activity_type_id,activity_name,activity_description,date_type,date_from_year,date_from_month,date_from_day,date_from_uncertainty,date_to_year,date_to_month,date_to_day,date_to_uncertainty,notes_used,additional_notes,creation_timestamp,creation_user,change_timestamp,change_user,event_label
    wr = CSVReader("pro_location.csv")
    headers = wr.next
    h = headers.zipWithIndex.toMap
    for (w <- wr) {
      val e = R(ns+"activity_"+w(h("activity_id")))
      val l = R(getUUIDURI("cofk_union_location-"+w(h("location_id"))))
      e.addProperty(CIDOC.took_place_at,l)
    }
//id,location_id,change_timestamp,activity_id
    wr = CSVReader("pro_primary_person.csv")
    headers = wr.next
    h = headers.zipWithIndex.toMap
    val activityPrimaryPersonNameMap = new HashMap[String,String]
    for (w <- wr)
      if (!personNameMap.contains(w(h("person_id")))) logger.warn("Unknown person: "+w.toSeq)
      else {
        val e = R(ns+"activity_"+w(h("activity_id")))
        e.addProperty(SKOS.prefLabel,activityTypeNameMap(w(h("activity_id")))+" of "+personNameMap(w(h("person_id"))),"en")
        val p = personIdMap(w(h("person_id")))
        activityPrimaryPersonNameMap.put(w(h("activity_id")),personNameMap(w(h("person_id"))))
        e.addProperty(CIDOC.had_participant,p)
      }
//id,person_id,change_timestamp,activity_id
    wr = CSVReader("pro_role_in_activity.csv")
    headers = wr.next
    h = headers.zipWithIndex.toMap
    for (w <- wr) if (!personIdMap.contains(w(h("entity_id")))) logger.warn("Unknown entity: "+w.toSeq) else {
      val a = personIdMap(w(h("entity_id")))
      val r = R(sns+"role_"+w(h("role_id")))
      val roleName = unCamelCase(w(h("role_id")))
      val e = R(ns+"activity_"+w(h("activity_id")))
      val as = I(ns+"role_in_activity_"+w(h("id")),personNameMap(w(h("entity_id")))+" as "+roleName+" in the "+activityTypeNameMap(w(h("activity_id")))+" of "+activityPrimaryPersonNameMap(w(h("activity_id"))),PROV.Association)
      as.addProperty(PROV.agent,a)
      as.addProperty(PROV.hadRole,r)
      e.addProperty(PROV.qualifiedAssociation,as)
      e.addProperty(CIDOC.had_participant,a)
    }
//id,entity_type,entity_id,role_id,change_timestamp,activity_id
    wr = CSVReader("pro_relationship.csv")
    headers = wr.next
    h = headers.zipWithIndex.toMap
    for (w <- wr) {
      val s = personIdMap(w(h("subject_id")))
      val o = personIdMap(w(h("object_id")))
      s.addProperty(P(sns+w(h("relationship_id"))),o)
    }
    wr = CSVReader("pro_textual_source.csv")
    headers = wr.next
    h = headers.zipWithIndex.toMap
    val sourceNameMap = new HashMap[String,String]
    for (w <- wr) {
      val s = I(ns+"source_"+w(h("id")),w(h("title")),CIDOC.Document)
      if(!w(h("author")).isEmpty)
        s.addProperty(DCTerms.creator,I(ns+"person_"+encode(w(h("author"))),w(h("author")),CIDOC.Person))
      val title = w(h("title")).trim
      sourceNameMap.put(w(h("id")),title)
      ANE(s,DCTerms.title,title)
      ANE(s,P(sns+"chapterArticleTitle"),w(h("chapterArticleTitle")))
      ANE(s,BIBO.volume,w(h("volumeSeriesNumber")))
      ANE(s,BIBO.issue,w(h("issueNumber")))
      ANE(s,BIBO.pages,w(h("pageNumber")))
      if (!w(h("editor")).isEmpty)
        s.addProperty(BIBO.editor,I(ns+"person_"+encode(w(h("editor"))),w(h("editor")),CIDOC.Person))
      ANE(s,DCTerms.date,w(h("datePublication")))
      ANE(s,FOAF.page,w(h("urlResource")))
      ANE(s,BIBO.shortTitle,w(h("abbreviation")))
      ANE(s,DCTerms.description,w(h("fullBibliographicDetails")))
      ANE(s,BIBO.edition,w(h("edition")))
      ANE(s,P(sns+"reprintFacsimile"),w(h("reprintFacsimile")))
      ANE(s,P(sns+"repository"),w(h("repository")))
    }
//id,author,title,chapterArticleTitle,volumeSeriesNumber,issueNumber,pageNumber,editor,
//placePublication,datePublication,urlResource,abbreviation,fullBibliographicDetails,
//edition,reprintFacsimile,repository,creation_user,creation_timestamp,change_user,
//change_timestamp

//id,subject_id,subject_type,subject_role_id,relationship_id,object_id,
//object_type,object_role_id,change_timestamp,activity_id
    wr = CSVReader("pro_assertion.csv")
    headers = wr.next
    h = headers.zipWithIndex.toMap
    for (w <- wr) {
      val a = R(ns+"activity_"+w(h("assertion_id")))
      val s = if (!w(h("source_description")).trim.isEmpty) {
        val s = I(ns+"source_"+w(h("source_id"))+"_"+encode(w(h("source_description")).trim),sourceNameMap(w(h("source_id")))+", "+w(h("source_description")),CIDOC.Document)
        s.addProperty(DCTerms.description,w(h("source_description")))
        s.addProperty(DCTerms.isPartOf,R(ns+"source_"+w(h("source_id"))))
      } else R(ns+"source_"+w(h("source_id")))
      a.addProperty(CIDOC.is_documented_in,s)
    }
//id,assertion_type,assertion_id,source_id,source_description,change_timestamp
    m.setNsPrefixes(PrefixMapping.Standard)
    m.setNsPrefix("org",ORG.ns)
    m.setNsPrefix("dcterms",DCTerms.NS)
    m.setNsPrefix("crm",CIDOC.ns)
    m.setNsPrefix("skos",SKOS.ns)
    m.setNsPrefix("emlo",ns)
    m.setNsPrefix("emlos",sns)
    RDFDataMgr.write(new FileOutputStream("emlo.ttl"), m, RDFFormat.TTL)
  }
}
