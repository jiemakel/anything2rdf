import org.apache.jena.rdf.model.ResourceFactory

/**
 * @author jiemakel
 *
 */

object BIBO {
  val ns = "http://purl.org/ontology/bibo/"
  def R(s: String) = ResourceFactory.createResource(ns+s)
  def P(s: String) = ResourceFactory.createProperty(ns+s)
  val volume = P("volume")
  val editor = P("editor")
  val pages = P("pages")
  val edition = P("edition")
  val issue = P("issue")
  val shortTitle = P("shortTitle")
}
