package com.prateek.xmlcompare.verify

import java.io.File

import com.prateek.xmlcompare.config.{MVC, VerificationConfig, VerifierId, YamlReader}
import com.prateek.xmlcompare.read.{DiscoverResponse, Message}
import com.prateek.xmlcompare.verify.XPathFactory.XPath

// Confirms if a [[Verifier]] can verify a [[XPath]]
trait VerificationPredicate:
  def apply(msg: Message, vd: VerifierId, xp: XPath): Boolean

object VerificationPredicate:
  def apply(config: File): VerificationPredicate = new RegexVerificationPredicate(
    //    TODO: this can also be considered to be moved to RegexVerificationPredicate but deferring this now
    vc = YamlReader(config)
  )

class RegexVerificationPredicate(vc: VerificationConfig) extends VerificationPredicate:
  override def apply(msg: Message, vid: VerifierId, xmlXPath: XPath): Boolean =
    // checks if config provided xpath matches the xml file xpath
    def matches(cxp: XPath): Boolean =
      val regex = cxp.r
      regex.matches(xmlXPath)
    end matches

    msg match
      case DiscoverResponse =>
        val matchingMVCsOpt: Seq[MVC] = vc.discoverResponse.collect({
          case mvc@(configXpath: XPath, _) if matches(configXpath) => mvc
        })
        val mostSpecificMVC: Option[MVC] = matchingMVCsOpt.maxByOption(_._1)
        mostSpecificMVC.exists(_._2.contains(vid))
      case _                => ???
  end apply
end RegexVerificationPredicate

/*
object RegexVerificationPredicate

def apply(config: File): RegexVerificationPredicate =
  VerificationConfigReader
 */
