package com.prateek.xmlcompare.verify

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*

import com.prateek.xmlcompare.config.VerifierId
import com.prateek.xmlcompare.read.{DiscoverResponse, Message}
import com.prateek.xmlcompare.verify.XPathFactory.XPath

class TextVerifierSpec extends AnyFunSpec {
  private val vp: VerificationPredicate = (_: Message, _: VerifierId, _: XPath) => true

  describe("comparing two matching nodes") {
    it("should return Match") {
      val exp = <Node>"test for matching node</Node>
      val act = <Node>"test for matching node</Node>
      val ctx: VerificationContext = VerificationContext(DiscoverResponse).append(exp)
      assertResult(Match)(TextVerifier(vp)(exp.child.head, act.child.head)(using ctx))
    }
  }

  describe("nodes mismatch") {
    it("should return Match as verifier will come across Elem and not Text") {
      val exp = <Node1>"test for non-matching node</Node1>
      val act = <Node2>"test for non-matching node</Node2>
      val ctx: VerificationContext = VerificationContext(DiscoverResponse)
      TextVerifier(vp)(exp, act)(using ctx) shouldBe Match
    }
  }

  describe("nodes match but the texts do not match") {
    it("should return NodeTextNotFound") {
      val exp = <Node1>text1</Node1>
      val act = <Node1>text2</Node1>
      val ctx: VerificationContext = VerificationContext(DiscoverResponse).append(exp)
      TextVerifier(vp)(exp.child.head, act.child.head)(using ctx) shouldBe NodeTextNotFound(
        "Node1\\%TEXT"
      )
    }
  }
}
