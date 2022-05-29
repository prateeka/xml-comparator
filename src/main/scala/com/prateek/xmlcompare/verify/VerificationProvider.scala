package com.prateek.xmlcompare.verify

trait VerificationProvider {

  /** Provides Verifier for the supplied xmlnode tag. Node tag format should be "n1.n2.n3"
    * where "n1" is the top node and "n3" is the deepest.
    */
  def apply(nt: String): Seq[Verification]
}

object VerificationProvider {
  val default: VerificationProvider = _ => Seq(LabelVerification)
}
