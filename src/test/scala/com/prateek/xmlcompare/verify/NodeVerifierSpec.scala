package com.prateek.xmlcompare.verify

import scala.xml.Node

import org.scalatest.funspec.AnyFunSpec

import com.prateek.xmlcompare.verify.VerifierId

class NodeVerifierSpec extends AnyFunSpec {
  val mv: Verifier = new Verifier {
    override val id: VerifierId = VerifierId.Node

    override def apply(exp: Node, act: Node)(using
        ctx: VerificationContext
    ): VerificationResult = {
      Match
    }
  }

  val mmv: Verifier = new Verifier {
    override val id: VerifierId = VerifierId.Node
    override def apply(exp: Node, act: Node)(using
        ctx: VerificationContext
    ): VerificationResult = {
      NodeNotFound("Node1")
    }
  }

  it("test for matching nodes") {
    given ctx: VerificationContext = VerificationContext()
    val exp: Node = <Node>"test for matching node</Node>
    val act: Node = <Node>"test for matching node</Node>
    val result = NodeVerifier(Seq(mv)).apply(exp, act)
    assertResult(Match)(result)
  }

  it("test for non-matching nodes") {
    given ctx: VerificationContext = VerificationContext()
    val exp = <Node1>"test for non-matching node</Node1>
    val act = <Node2>"test for non-matching node</Node2>
    val result = NodeVerifier(Seq(mv, mmv)).apply(exp, act)
    assertResult(NodeNotFound("Node1"))(result)
  }
}
