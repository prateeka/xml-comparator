package com.prateek.xmlcompare.verify

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*

class LabelTextVerifierSpec extends AnyFunSpec {

  describe("comparing two matching nodes") {
    it("should return Match") {
      val exp = <Node>"test for matching node</Node>
      val act = <Node>"test for matching node</Node>
      val ctx: VerificationContext = VerificationContext().append(exp)
      assertResult(Match)(LabelTextVerifier(exp, act)(using ctx))
    }
  }

  describe("nodes mismatch") {
    it("should return NodeNotFound with missing node") {
      val exp = <Node1>"test for non-matching node</Node1>
      val act = <Node2>"test for non-matching node</Node2>
      val ctx: VerificationContext = VerificationContext()
      LabelTextVerifier(exp, act)(using ctx) shouldBe NodeNotFound("Node1")
    }
  }

  describe("nodes match but the texts do not match") {
    it("should return NodeTextNotFound") {
      val exp = <Node1>text1</Node1>
      val act = <Node1>text2</Node1>
      val ctx: VerificationContext = VerificationContext().append(exp)
      LabelTextVerifier(exp.child.head, act.child.head)(using ctx) shouldBe NodeTextNotFound(
        "Node1\\%TEXT"
      )
    }
  }
}
