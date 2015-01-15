
import com.hp.hpl.jena.rdf.model.ResourceFactory

/**
 * @author jiemakel
 *
 */

object REL {
  val ns = "http://purl.org/vocab/relationship/"
  def R(s: String) = ResourceFactory.createResource(ns+s)
  def P(s: String) = ResourceFactory.createProperty(ns+s)
val acquaintanceOf = P("acquaintanceOf")
val ambivalentOf = P("ambivalentOf")
val ancestorOf = P("ancestorOf")
val antagonistOf = P("antagonistOf")
val apprenticeTo = P("apprenticeTo")
val childOf = P("childOf")
val closeFriendOf = P("closeFriendOf")
val collaboratesWith = P("collaboratesWith")
val colleagueOf = P("colleagueOf")
val descendantOf = P("descendantOf")
val employedBy = P("employedBy")
val employerOf = P("employerOf")
val enemyOf = P("enemyOf")
val engagedTo = P("engagedTo")
val friendOf = P("friendOf")
val grandchildOf = P("grandchildOf")
val grandparentOf = P("grandparentOf")
val hasMet = P("hasMet")
val influencedBy = P("influencedBy")
val knowsByReputation = P("knowsByReputation")
val knowsInPassing = P("knowsInPassing")
val knowsOf = P("knowsOf")
val lifePartnerOf = P("lifePartnerOf")
val livesWith = P("livesWith")
val lostContactWith = P("lostContactWith")
val mentorOf = P("mentorOf")
val neighborOf = P("neighborOf")
val parentOf = P("parentOf")
val participant	= P("participant")
val participantIn	= P("participantIn")
val Relationship	= R("Relationship")
val siblingOf = P("siblingOf")
val spouseOf = P("spouseOf")
val worksWith = P("worksWith")
val wouldLikeToKnow = P("wouldLikeToKnow")
}
