
import com.hp.hpl.jena.rdf.model.ResourceFactory

/**
 * @author jiemakel
 *
 */

object PROV {
  val ns = "http://www.w3.org/ns/prov#"
  def R(s: String) = ResourceFactory.createResource(ns+s)
  def P(s: String) = ResourceFactory.createProperty(ns+s)
  val qualifiedAssociation = P("qualifiedAssociation")
  val hadRole = P("hadRole")
  val Role = R("Role")
  val Association = R("Association")
  val agent = P("agent")
}
