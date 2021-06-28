package com.prateek.xmlcompare.verify

trait VerificationProvider {

  /** Provides Verifier for the supplied node string. Node String format should be "n1.n2.n3"
    * where "n1" is the top node and "n3" is the deepest.
    */
  def apply(ns: String): Seq[Verification]
}

object VerificationProvider {
  val default: VerificationProvider = _ => Seq(LabelVerification)
}
