package com.prateek.xmlcompare.verify

import com.prateek.xmlcompare.config.{MVC, VerificationConfig, VerifierId}
import com.prateek.xmlcompare.read.{DiscoverResponse, Message}
import com.prateek.xmlcompare.verify.XPathFactory.XPath

// Confirms if a [[Verifier]] can verify a [[XPath]]
trait VerificationPredicate:
  def apply(msg: Message, vd: VerifierId, xp: XPath): Boolean

object VerificationPredicate:
  val instance: VerificationPredicate = new RegexVerificationPredicate(
    VerificationConfig(Nil: Seq[MVC])
  )

class RegexVerificationPredicate(vc: VerificationConfig) extends VerificationPredicate:
  override def apply(msg: Message, vid: VerifierId, xp: XPath): Boolean =
    // checks if config provided xpath matches the xml file xpath
    def matches(cxp: XPath): Boolean =
      val regex = cxp.r
      regex.matches(xp)
    end matches

    msg match
      case DiscoverResponse =>
        val matchingVerifierIds = vc.discoverResponse.collect({
          case (configXpath: XPath, vids) if matches(configXpath) => vids
        })
        val minMatchingVerifiers = matchingVerifierIds.minBy(_.size)
        minMatchingVerifiers.contains(vid)
      case _ => ???
  end apply
end RegexVerificationPredicate
