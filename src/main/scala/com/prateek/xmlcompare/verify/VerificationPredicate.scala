package com.prateek.xmlcompare.verify

import com.prateek.xmlcompare.read.{DiscoverResponse, Message}
import com.prateek.xmlcompare.verify.Identification.VerifierId
import com.prateek.xmlcompare.verify.XPathFactory.XPath

transparent trait Identification:
  val id: VerifierId

object Identification:
  type VerifierId = String

// Confirms if a [[Verifier]] can verify a [[XPath]]
trait VerificationPredicate:
  def apply(msg: Message, vd: VerifierId, xp: XPath): Boolean

object VerificationPredicate:
  val instance: VerificationPredicate = ???

class RegexVerificationPredicate(vc: VerificationConfig) extends VerificationPredicate:
  override def apply(msg: Message, vid: VerifierId, xp: XPath): Boolean =
    msg match
      case DiscoverResponse => ???
      case _                => ???
