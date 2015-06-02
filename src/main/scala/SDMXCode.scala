import com.hp.hpl.jena.rdf.model.ResourceFactory

/**
 * @author jiemakel
 *
 */

object SDMXCode {
  val ns = "http://purl.org/linked-data/sdmx/2009/code#"
  def R(s: String) = ResourceFactory.createResource(ns+s)
  def P(s: String) = ResourceFactory.createProperty(ns+s)
  val sexMale = R("sex-M")
  val sexFemale = R("sex-F")
}
