
import com.hp.hpl.jena.rdf.model.ResourceFactory

/**
 * @author jiemakel
 *
 */

object WGS84 {
  val ns = "http://www.w3.org/2003/01/geo/wgs84_pos#"
  def R(s: String) = ResourceFactory.createResource(ns+s)
  def P(s: String) = ResourceFactory.createProperty(ns+s)
  val lat = P("lat")
  val long = P("long")
}
