
import com.hp.hpl.jena.rdf.model.ResourceFactory

/**
 * @author jiemakel
 *
 */

object SKOS {
  val ns = "http://www.w3.org/2004/02/skos/core#"
  def R(s: String) = ResourceFactory.createResource(ns+s)
  def P(s: String) = ResourceFactory.createProperty(ns+s)
  val Concept = R("Concept")
  val broader = P("broader")
  val prefLabel = P("prefLabel")
  val altLabel = P("altLabel")
  val hiddenLabel = P("hiddenLabel")
  val narrower = P("narrower")
  val definition = P("definition")
  val notation = P("notation")
  val member = P("member")
  val related = P("related")
  val note = P("note")
  val broaderTransitive = P("broaderTransitive")
  val narrowerTransitive = P("narrowerTransitive")
  val exactMatch = P("exactMatch")
  val skosMappingExactMatch = ResourceFactory.createProperty("http://www.w3.org/2004/02/skos/mapping#exactMatch")
  val scopeNote = P("scopeNote")
}
