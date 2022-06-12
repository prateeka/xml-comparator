package com.prateek.xmlcompare.verify

import java.io.File

trait VerificationResult

object VerificationResult {
  given vo: Ordering[VerificationResult] with
    override def compare(
        xvr: VerificationResult,
        yvr: VerificationResult
    ): Int = (xvr, yvr) match
      case (_ @Match, _: VerificationResult) => 1
      case (_: VerificationResult, _ @Match) => -1
      case (a: Mismatch, b: Mismatch) =>
        a.label().length.compare(b.label().length)
}

case object Match extends VerificationResult

trait Mismatch(n: String) extends VerificationResult {
  def label(): String = n
}

case class NodeNotFound(n: String) extends Mismatch(n)

case class NodeTextNotFound(n: String) extends Mismatch(n)

case class FileVerificationResult(
    exp: File,
    act: File,
    verificationResult: VerificationResult
)
