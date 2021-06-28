package com.prateek.xmlcompare.verify

import scala.xml.Node

import org.scalatest.funspec.AnyFunSpec

class NodeVerificationTest extends AnyFunSpec {

  val mv: Verification = new Verification {
    override def apply(exp: Node, act: Node)(using
        ctx: Context
    ): VerificationResult = {
      Match
    }
  }

  val mmv: Verification = new Verification {
    override def apply(exp: Node, act: Node)(using
        ctx: Context
    ): VerificationResult = {
      NodeNotFound("Node1")
    }
  }

  given ctx: Context = Context()

  describe("test NodeVerification") {

    it("test for matching nodes") {
      val exp: Node = <Node>"test for matching node</Node>
      val act: Node = <Node>"test for matching node</Node>
      val vp: VerificationProvider = (ns: String) => Seq(mv)
      val result = NodeVerification(vp).apply(exp, act)
      assertResult(Match)(result)
    }

    it("test for non-matching nodes") {
      val exp = <Node1>"test for non-matching node</Node1>
      val act = <Node2>"test for non-matching node</Node2>
      val vp: VerificationProvider = (ns: String) => Seq(mv, mmv)
      val result = NodeVerification(vp).apply(exp, act)
      assertResult(NodeNotFound("Node1"))(result)
    }
  }
}
