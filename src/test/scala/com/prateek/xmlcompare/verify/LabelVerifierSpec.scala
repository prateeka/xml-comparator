package com.prateek.xmlcompare.verify

import org.scalatest.funspec.AnyFunSpec

class LabelVerifierSpec extends AnyFunSpec {

  it("test for matching nodes") {
    val exp = <Node>"test for matching node</Node>
    val act = <Node>"test for matching node</Node>
    val ctx: VerificationContext = VerificationContext().append(exp)
    assertResult(Match)(LabelVerifier(exp, act)(using ctx))
  }

  it("test for non-matching nodes") {
    val exp = <Node1>"test for non-matching node</Node1>
    val act = <Node2>"test for non-matching node</Node2>
    val ctx: VerificationContext = VerificationContext().append(exp)
    assertResult(NodeNotFound("Node1"))(
      LabelVerifier(exp, act)(using ctx)
    )
  }
}
