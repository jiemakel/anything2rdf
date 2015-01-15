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
import com.hp.hpl.jena.sparql.vocabulary.FOAF
import com.github.nscala_time.time.Imports._
import org.joda.time.format.ISODateTimeFormat
import com.hp.hpl.jena.shared.PrefixMapping
import com.hp.hpl.jena.datatypes.xsd.XSDDatatype
import com.hp.hpl.jena.vocabulary.DCTerms
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import scala.util.control.Breaks._
import com.hp.hpl.jena.rdf.model.Property
import com.bizo.mighty.csv.CSVReaderSettings

object SCHOENBERGCSV2RDF extends CSV2RDF {
  
  val sns = "http://ldf.fi/schoenberg/schema#"
  val ns = "http://ldf.fi/schoenberg/"

  object EventType extends Enumeration {
    type EventType = Value
    val Sale,Gift,NoSale,Possession = Value
  }
  
  val Auction = EC("Auction")
  Auction.addProperty(RDFS.subClassOf, CIDOC.Event)

  val Possession = EC("Possession")
  Possession.addProperty(RDFS.subClassOf, CIDOC.Event)
  
  val Gift = EC("Gift")
  Gift.addProperty(RDFS.subClassOf, CIDOC.Transfer_of_Custody)
  val Sale = EC("Sale")
  Sale.addProperty(RDFS.subClassOf, CIDOC.Transfer_of_Custody)
  
  val CatalogueEntry = EC("Catalogue Entry")
  
  val Catalogue = EC("Catalogue")
  val Currency = EC("Currency")
  
  val Manuscript = EC("Manuscript")
  
  val Work = EC("Work")
  
  val catalogueEntry = EOP("catalogueEntry")
  
  val price = EDP("price")

  val currency = EOP("currency")
  
  val sold = EDP("sold")
  
  val represents = EOP("represents")
  
  val language = EOP("language")
  
  val decorated_initials = EDP("decorated initials")
  val historiated_initials = EDP("historiated initials")
  
  val date = EOP("data")
  val seller = EOP("seller")
  val artist = EOP("artist")
  val scribe = EOP("scribe")
  val buyer = EOP("buyer")
  val soldOnBehalfOf = EOP("put on sale on behalf of")
  
  val folios = EDP("folios")
  val columns = EDP("columns")
  val lines = EDP("lines")
  val height = EDP("height")
  val binding = EDP("binding")
  val width = EDP("width")
  
  val circa = EDP("circa") //FIXME
  val current_location = EDP("current location") //FIXME
  
  val liturgicalUse = EOP("liturgical use")
  
  val possiblySameAs = EOP("possibly same as")
  
  val small_miniatures = EDP("small miniatures")
  val large_miniatures = EDP("large miniatures")
  val fullpage_miniatures = EDP("full-page miniatures")
  val unspecified_miniatures = EDP("unspecified miniatures")
  
  val provenance = EOP("provenance")
  
  val material = EOP("material")
  val Vellum = I(ns+"material_vellum",Map("en"->"Vellum"),CIDOC.Material)
  val Paper = I(ns+"material_paper",Map("en"->"Paper"),CIDOC.Material)

  val mmap = new HashMap[String,String]
  
  def main(args: Array[String]): Unit = {
    var wr : CSVReader = null
    var headers: Array[String] = null
    var h: Map[String,Int] = null
    wr = CSVReader("schoenberg.csv")(CSVReaderSettings.Standard.copy(escapechar='\0'))
    headers = wr.next
    h = headers.zipWithIndex.toMap
    println(headers.toSeq)
/*
 * MANUSCRIPT_ID, DUPLICATE_MS, CAT_DATE, SELLER, SELLER2, INSTITUTION, BUYER, CAT_ID, CAT_OR_LOT_NUM, PRICE, CURRENCY, SOLD, 
 * SECONDARY_SOURCE, CURRENT_LOCATION, AUTHOR_AUTHORITY, AUTHOR_VARIANT, TITLE, LNG, MAT, PLACE, MANUSCRIPT_USE, MANUSCRIPT_DATE, 
 * CIRCA, ARTIST, SCRIBE, FOLIOS, COL, LINES, HGT, WDT, MANUSCRIPT_BINDING, PROVENANCE, COMMENTS, MANUSCRIPT_LINK, MIN_FL, MIN_LG, MIN_SM,
 * MIN_UN, H_INIT, D_INIT, ENTRY_COMMENTS, ADDEDON, ADDEDBY, ISAPPROVED, ISDELETED, LAST_MODIFIED, LAST_MODIFIED_BY, POSSIBLE_DUPS, 
 * POSSIBLE_DUPS, X
 * 
List(219, 219,3401,17536,189079, 19970617, Sotheby's, , , , Western manuscripts and miniatures (LN7375), 62, 71445, , , , , Guillaume de Lorris|Jean de Meun, Guillaume de Lorris|Jean de Meung, Roman de la Rose, French, V, France, , 1425, c1h+, , , 172, 2, 34, 311, 260, , Royez|Hennier|Phillipps, Thomas, Sir, Phillipps129, , , , , , , , PROV-UPDATE-PHIL-1, 24-Sep-2009, import, , , 11-Aug-2011, posch, , , X)
List(220, , 19970617, Sotheby's, , , , Western manuscripts and miniatures (LN7375), 61, 27513.75, , , , , , , Statutes, Savoy, Latin, P, France, Savoy, , 1475, C2H, , , 200, 1, 40, 245, 180, , De Glos La Malme|De Guigne, , , , , , , , , , 24-Sep-2009, import, , , 14-Mar-2011, aldevine, , , X)
List(221, , 19970617, Sotheby's, , , , Western manuscripts and miniatures (LN7375), 60, 30360, , , , , , Jean D'orronville, Chronicles, Savoy, French, P, France, Savoy, , 1463, C3Q, , , 313, 1, 22, 303, 213, , Duke Savoy, , , , , , , , , , 24-Sep-2009, import, , , 14-Mar-2011, aldevine, , , X)
List(222, 222,6391,9885,9941,13883,13866,77928,97264, 19970617, Sotheby's, , , , Western manuscripts and miniatures (LN7375), 69, 60555, , , , , , , Breviary, Latin, V, Netherlands, Delft, Augustinian, 1460, C3Q, Master of the Fagel Missal, , 246, 2, 23, 168, 122, Late 18thc binding For Wodhull, Canoness Regular St Agnes-Delft|Askew, Anthony, 1722-1774|Wodhull|Severne|Ingram|Jacobsen|Busch|Gunther|Symonds, Exh. Brussels 1979-11, , 1, , 1, , 9, 16, , 24-Sep-2009, import, , , 27-Nov-2013, fraas, , , X)
 * 
 */
    var i = 1
    val midP = h("MANUSCRIPT_ID")
    val mdupidsP = h("DUPLICATE_MS")
    for (w <- wr) {
      val mid = w(midP)
      if (w.length!=50)
        println(i,w.length,mid)
      w(mdupidsP).split(",").map(_.trim).filter {!_.isEmpty } foreach { x => if (x<mmap.getOrElse(mid,mid)) mmap.put(mid,x) }      
      i+=1
    }
    for (k <- mmap.keySet) {
      var v = mmap.get(k)
      var lv = v
      while (lv!=None) {
        lv = mmap.get(v.get)
        if (lv!=None) v = lv
      }
      mmap.put(k,v.get)
    }
    wr = CSVReader("schoenberg.csv")(CSVReaderSettings.Standard.copy(escapechar='\0'))
    headers = wr.next
    h = headers.zipWithIndex.toMap
    println(headers.toSeq)
    breakable {for (w <- wr) {
      val mid = mmap.getOrElse(w(midP),w(midP))
      val ss = w(h("SOLD"))
      val date = "(\\d\\d\\d\\d)(\\d\\d)(\\d\\d)".r.replaceFirstIn(w(h("CAT_DATE")),"$1-$2-$3")
      val recipient = if (w(h("BUYER"))!="") w(h("BUYER")) else w(h("INSTITUTION"))
      val etype = if (ss=="" && w(h("SELLER"))=="" && w(h("INSTITUTION"))!="") EventType.Possession else if (ss=="GIFT") EventType.Gift else if (ss=="NO" || ss=="N?" || ss=="WD" || (recipient=="" && ss!="YES")) EventType.NoSale else EventType.Sale
      val auctionEvent = if (etype==EventType.Sale || etype==EventType.NoSale) Some(I(ns+"event_auction_"+mid+"_"+w(midP),Map("en"->("Auction of "+w(h("TITLE"))+" on "+date)),Auction)) else None
      val primarySeller = if (w(h("SELLER2"))!="") w(h("SELLER2")) else w(h("SELLER"))
      val transferEvent = if (etype==EventType.Sale || etype==EventType.Gift) Some(I(ns+"event_transfer_"+mid+"_"+w(midP),Map("en"->((if (etype==EventType.Gift) "Gift" else "Sale")+" of "+w(h("TITLE"))+" from "+primarySeller+" to "+recipient+" on "+date)),if (etype==EventType.Gift) Gift else Sale)) else None
      val beforePossessionEvent = if (etype!=EventType.Possession) Some(I(ns+"event_possession_"+mid+"_"+encode(primarySeller),Map("en"->("Possession by "+primarySeller+" of "+w(h("TITLE"))+" until "+date)),Possession)) else None
      val afterPossessionEvent = if (recipient!="") Some(I(ns+"event_possession_"+mid+"_"+encode(recipient),Map("en"->("Possession by "+recipient+" of "+w(h("TITLE"))+" after "+date)),Possession)) else None
      beforePossessionEvent.foreach(bpe => auctionEvent.orElse(transferEvent).foreach{ae => bpe.addProperty(CIDOC.occurs_before,ae);ae.addProperty(CIDOC.occurs_after,bpe)});
      transferEvent.foreach(te => auctionEvent.foreach(ae => te.addProperty(CIDOC.occurs_during,ae)))
      auctionEvent.orElse(transferEvent).foreach(ae => afterPossessionEvent.foreach{ape => ae.addProperty(CIDOC.occurs_before,ape);ape.addProperty(CIDOC.occurs_after,ae)});
      val c = I(ns+"catalogue_"+encode(w(h("CAT_ID"))),Map("en"->w(h("CAT_ID"))),Catalogue)
      val ce = I(ns+"catalogueEntry_"+mid+"_"+w(midP),Map("en"->(w(h("CAT_OR_LOT_NUM"))+": "+w(h("TITLE")))),CatalogueEntry)
      val manuscript = I(ns+"manuscript_"+mid,Map("en"->w(h("TITLE"))),Manuscript)
      if (mid!=w(midP)) manuscript.addProperty(OWL.sameAs,m.createResource(ns+"manuscript_"+w(midP)))
      val authorVariants = w(h("AUTHOR_VARIANT")).split("\\|").map(_.trim)
      val authorNames = {
        val t = w(h("AUTHOR_AUTHORITY")).split("\\|").map(_.trim)
        if (t.length!=0) t else authorVariants
      }
      val works = {
        val workNames = w(h("TITLE")).split("\\|").map(_.trim).filter { !_.isEmpty }
        if (workNames.length==authorNames.length) for (i <- 0 until workNames.length;workName = workNames(i);authorName = authorNames(i)) yield I(ns+"work_"+encode(authorName)+"_"+encode(workName),workName,Work)
        else workNames.map(s => I(ns+"work_"+mid+"_"+encode(s),s,Work)).toSeq
      }
      works.foreach(manuscript.addProperty(CIDOC.carries,_))
      for (ch <- h;s = w(ch._2)) if (s!="") ch._1 match {
        case "CAT_ID" => auctionEvent.orElse(transferEvent).orElse(afterPossessionEvent).orElse(beforePossessionEvent).getOrElse(throw new IllegalArgumentException("BAD: "+headers.zip(w).toMap)).addProperty(catalogueEntry,ce)
        case "CAT_OR_LOT_NUM" => ce.addProperty(DCTerms.isPartOf,c)
        case "CAT_DATE" => 
          val (p,e) = try {
            (CIDOC.has_timeSpan,Left(makeTimeSpan(s.substring(0,4)+"-"+s.substring(4,6)+'-'+s.substring(6),makeDateTime(s.substring(0,4), s.substring(4,6), s.substring(6)))))
          } catch {
            case ex: Exception => (DCTerms.date,Right(s))
          }
          e match {
            case Left(v) => auctionEvent.foreach(_.addProperty(p,v));transferEvent.foreach(_.addProperty(p,v))
            case Right(v) => auctionEvent.foreach(_.addProperty(p,v));transferEvent.foreach(_.addProperty(p,v))
          }
        case "MANUSCRIPT_DATE" =>  s.split("[\\|,]").filter(!_.isEmpty).foreach(s => try {
          manuscript.addProperty(CIDOC.has_timeSpan,makeTimeSpan(s,makeDateTime(s,"","")))
        } catch {
          case e : Exception => println("Bad date for manuscript",e);manuscript.addProperty(DCTerms.date,s)
        })
        case "MANUSCRIPT_ID" =>
          beforePossessionEvent.foreach(_.addProperty(CIDOC.transferred_title_of,manuscript))
          afterPossessionEvent.foreach(_.addProperty(CIDOC.transferred_title_of,manuscript))
          ce.addProperty(represents,manuscript)
        case "DUPLICATE_MS" => s.split(",").map(_.trim).filter { x => !x.isEmpty && x!=mid }.foreach { x => manuscript.addProperty(OWL.sameAs,m.createResource(ns+"manuscript_"+x)) }
        case "SELLER" => 
          beforePossessionEvent.foreach(_.addProperty(CIDOC.transferred_title_to,I(ns+"actor_"+encode(s),s,CIDOC.Actor)))
          transferEvent.foreach(_.addProperty(seller,I(ns+"actor_"+encode(s),s,CIDOC.Actor)))
        case "SELLER2" => 
          transferEvent.foreach(_.addProperty(CIDOC.transferred_title_from,I(ns+"actor_"+encode(s),s,CIDOC.Actor)))
        case "BUYER" => 
          afterPossessionEvent.foreach(_.addProperty(CIDOC.transferred_title_to,I(ns+"actor_"+encode(s),s,CIDOC.Actor)))
          transferEvent.foreach(_.addProperty(CIDOC.transferred_title_to,I(ns+"actor_"+encode(s),s,CIDOC.Actor)))
        case "INSTITUTION" => 
          afterPossessionEvent.foreach(_.addProperty(CIDOC.transferred_title_to,I(ns+"actor_"+encode(s),s,CIDOC.Legal_Body)))
          transferEvent.foreach(_.addProperty(CIDOC.transferred_title_to,I(ns+"actor_"+encode(s),s,CIDOC.Legal_Body)))
        case "ARTIST" => s.split("\\|").map(_.trim).filter {!_.isEmpty }.foreach { x => manuscript.addProperty(artist,I(ns+"actor_"+encode(x),x,CIDOC.Actor)) }
        case "SCRIBE" => s.split("\\|").map(_.trim).filter {!_.isEmpty }.foreach { x => manuscript.addProperty(scribe,I(ns+"actor_"+encode(x),x,CIDOC.Actor)) }
        case "AUTHOR_AUTHORITY" => 
          if (works.length==authorNames.length) for (i <- 0 until works.length;work = works(i);a = authorNames(i);if !a.isEmpty) work.addProperty(DCTerms.creator,I(ns+"actor_"+encode(a),a,CIDOC.Person))
          else authorNames.foreach { a => manuscript.addProperty(DCTerms.creator,I(ns+"actor_"+encode(a),a,CIDOC.Person)) }
        case "AUTHOR_VARIANT" => if (authorNames.length==authorVariants.length) for (i <- 0 until authorNames.length;a = authorNames(i);av = authorVariants(i);if a!=av && !av.isEmpty) I(ns+"actor_"+encode(a),a,CIDOC.Person).addProperty(SKOS.altLabel,av)
          else println("lost variants: "+authorVariants.toSeq+" vs "+authorNames.toSeq)
        case "COMMENTS" => auctionEvent.orElse(transferEvent).orElse(afterPossessionEvent).orElse(beforePossessionEvent).getOrElse(throw new IllegalArgumentException("BAD: "+headers.zip(w).toMap)).addProperty(RDFS.comment,s)
        case "SOLD" => s match {
          case "NO" | "N?" | "GIFT" =>
          case "YES" => auctionEvent.foreach(_.addProperty(sold,"true",XSDDatatype.XSDboolean))
          case "WD" => auctionEvent.foreach(_.addProperty(sold,"withdrawn"))
          case s => auctionEvent.foreach(_.addProperty(sold,s)); println("Unknown sold: "+s)
        } 
        case "PROVENANCE" => 
          var list = m.createResource()
          beforePossessionEvent.orElse(afterPossessionEvent).foreach{e =>
            var provs = s.split("\\|").map(_.trim).filter(!_.isEmpty)
            if (provs.last==primarySeller) provs=provs.dropRight(1)
            if (!provs.isEmpty) {
              val pe = provs.foldLeft(null.asInstanceOf[Resource]) { case (r,s) =>
                val e = I(ns+"event_possession_"+mid+"_"+encode(s),Map("en"->("Possession by "+s+" of "+w(h("TITLE")))),Possession)
                e.addProperty(CIDOC.transferred_title_of,manuscript)
                e.addProperty(CIDOC.transferred_title_to,I(ns+"actor_"+encode(s),s,CIDOC.Actor))
                if (r!=null) {
                  r.addProperty(CIDOC.occurs_before,e) 
                  e.addProperty(CIDOC.occurs_after,r)
                }
                e
              }
              pe.addProperty(CIDOC.occurs_before,e)
              e.addProperty(CIDOC.occurs_after,pe)
            }
          }
        case "MAT" => s.split("[\\|,]").map(_.trim).filter(!_.isEmpty).foreach {
          case "V" | "Vellum" => manuscript.addProperty(material,Vellum)
          case "P" | "Paper" => manuscript.addProperty(material,Paper)
          case "PV" | "VP" => manuscript.addProperty(material,Vellum);manuscript.addProperty(material,Paper)
          case "" | "|" | "," =>
          case s => manuscript.addProperty(material,I(ns+"material_"+encode(s),Map("en"->s),CIDOC.Material))
        }
        case "MANUSCRIPT_BINDING" => manuscript.addProperty(binding,s)
        case "LNG" => 
          val lngs = s.split("[\\|,]").map(_.trim).filter(!_.isEmpty)
          if (works.length==lngs.length) for (i <- 0 until works.length;work = works(i);l = lngs(i)) work.addProperty(DCTerms.language,I(ns+"language_"+encode(l),Map("en"->l),CIDOC.Language))
          else lngs.foreach { l => manuscript.addProperty(DCTerms.language,I(ns+"language_"+encode(l),Map("en"->l),CIDOC.Language)) }
        case "FOLIOS" => manuscript.addProperty(folios,s,XSDDatatype.XSDinteger)
        case "COL" => manuscript.addProperty(columns,s,XSDDatatype.XSDinteger)
        case "LINES" => manuscript.addProperty(lines,s,XSDDatatype.XSDinteger)
        case "HGT" => manuscript.addProperty(height,s,XSDDatatype.XSDinteger)
        case "WDT" => manuscript.addProperty(width,s,XSDDatatype.XSDinteger)
        case "H_INIT" => manuscript.addProperty(historiated_initials,s,XSDDatatype.XSDinteger)
        case "D_INIT" => manuscript.addProperty(decorated_initials, s,XSDDatatype.XSDinteger)
        case "MIN_FL" => manuscript.addProperty(fullpage_miniatures,s,XSDDatatype.XSDinteger)
        case "MIN_SM" => manuscript.addProperty(small_miniatures,s,XSDDatatype.XSDinteger)
        case "MIN_LG" => manuscript.addProperty(large_miniatures,s,XSDDatatype.XSDinteger)
        case "MIN_UN" => manuscript.addProperty(unspecified_miniatures,s,XSDDatatype.XSDinteger)
        case "PRICE" => ce.addProperty(price,s,XSDDatatype.XSDdecimal)
        case "CURRENCY" => 
          if (s.matches("[A-Z]+")) ce.addProperty(currency,I(ns+"currency_"+encode(s),s,Currency))
          else ce.addProperty(currency,s)
        case "ENTRY_COMMENTS" => auctionEvent.orElse(transferEvent).orElse(afterPossessionEvent).orElse(beforePossessionEvent).getOrElse(throw new IllegalArgumentException("BAD: "+headers.zip(w).toMap)).addProperty(RDFS.comment,s,"en")
        case "SECONDARY_SOURCE" => auctionEvent.orElse(transferEvent).orElse(afterPossessionEvent).orElse(beforePossessionEvent).getOrElse(throw new IllegalArgumentException("BAD: "+headers.zip(w).toMap)).addProperty(SKOS.scopeNote,s,"en")
        case "TITLE" | "X" => 
        case "MANUSCRIPT_LINK" => manuscript.addProperty(FOAF.page,ResourceFactory.createResource(s))
        case "PLACE" => manuscript.addProperty(CIDOC.took_place_at,I(ns+"location_"+encode(s),Map("en"->s),CIDOC.Place))
        case "MANUSCRIPT_USE" => s.split("\\|").map(_.trim).filter {!_.isEmpty }.foreach { s => manuscript.addProperty(liturgicalUse,I(ns+"location_"+encode(s),Map("en"->s),CIDOC.Place)) }
        case "POSSIBLE_DUPS" => s.split(",").map(_.trim).filter {!_.isEmpty }.foreach { x => manuscript.addProperty(possiblySameAs,m.createResource(ns+"manuscript_"+x)) }
        case "CIRCA" => manuscript.addProperty(circa,s) //FIXME
        case "CURRENT_LOCATION" => manuscript.addProperty(current_location,s) //FIXME
        case "ISAPPROVED" | "ADDEDON" | "ADDEDBY" | "LAST_MODIFIED" | "LAST_MODIFIED_BY" => auctionEvent.orElse(transferEvent).orElse(afterPossessionEvent).orElse(beforePossessionEvent).getOrElse(throw new IllegalArgumentException("BAD: "+headers.zip(w).toMap)).addProperty(EDP(ch._1),s);
        case p => println(p+":"+s); auctionEvent.orElse(transferEvent).orElse(afterPossessionEvent).orElse(beforePossessionEvent).getOrElse(throw new IllegalArgumentException("BAD: "+headers.zip(w).toMap)).addProperty(EDP(p),s); 
      }
      i+=1
    }}
//MANUSCRIPT_ID, DUPLICATE_MS, CAT_DATE, SELLER, SELLER2, INSTITUTION, BUYER, CAT_ID, CAT_OR_LOT_NUM, PRICE, CURRENCY, SOLD, SECONDARY_SOURCE, CURRENT_LOCATION, AUTHOR_AUTHORITY, AUTHOR_VARIANT, TITLE, LNG, MAT, PLACE, MANUSCRIPT_USE, MANUSCRIPT_DATE, CIRCA, ARTIST, SCRIBE, FOLIOS, COL, LINES, HGT, WDT, MANUSCRIPT_BINDING, PROVENANCE 1, PROVENANCE 2, PROVENANCE 3, PROVENANCE 4, PROVENANCE 5, PROVENANCE 6, PROVENANCE 7, PROVENANCE 8, PROVENANCE 9, PROVENANCE 10, PROVENANCE 11, PROVENANCE 12, PROVENANCE 13, PROVENANCE 14, PHILLIPPS_NUM, COMMENTS 1, COMMENTS 2, COMMENTS 3, COMMENTS 4, COMMENTS 5, COMMENTS 6, COMMENTS 7, COMMENTS 8, COMMENTS 9, COMMENTS 10, COMMENTS 11, COMMENTS 12, COMMENTS 13, COMMENTS 14, COMMENTS 15, COMMENTS 16, COMMENTS 17, COMMENTS 18, COMMENTS 19, MANUSCRIPT_LINK    
    m.setNsPrefixes(PrefixMapping.Standard)
    m.setNsPrefix("org",ORG.ns)
    m.setNsPrefix("dcterms",DCTerms.NS)
    m.setNsPrefix("crm",CIDOC.ns)
    m.setNsPrefix("skos",SKOS.ns)
    m.setNsPrefix("s",ns)
    m.setNsPrefix("ss",sns)
    RDFDataMgr.write(new FileOutputStream("schoenberg.ttl"), m, RDFFormat.TTL)
  }
}
