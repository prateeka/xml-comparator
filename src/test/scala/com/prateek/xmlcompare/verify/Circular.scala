package com.prateek.xmlcompare.verify

object Main extends App {

  def method(): Unit = {
    lazy val dvp: VerificationProvider = new DummyVp(cv)
    lazy val nv = NodeVerifier(dvp)
    lazy val cv: ChildVerifier = ChildVerifier(nv)
  }

  class DummyVp(cv1: => ChildVerifier) extends VerificationProvider {
    lazy val cv: ChildVerifier = cv1

    override def apply(nt: String): Seq[Verifier] = {
      Seq(LabelVerifier, cv)
    }
  }
}
