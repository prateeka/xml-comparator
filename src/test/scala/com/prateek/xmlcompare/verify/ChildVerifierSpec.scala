package com.prateek.xmlcompare.verify

import scala.xml.Node

import org.scalatest.funspec.AnyFunSpec

import com.prateek.xmlcompare.read.trim
import com.prateek.xmlcompare.verify.Identification.VerifierId

class ChildVerifierSpec extends AnyFunSpec {

  private val mv = new Verifier {
    override val id: VerifierId = "dummy"
    override def apply(exp: Node, act: Node)(using
        ctx: VerificationContext
    ): VerificationResult = {
      val vr =
        if exp.equals(act) then Match
        else NodeNotFound(exp.label)
      vr
    }
  }

  describe("when all the expected nodes match subset of actual nodes") {
    it("should return Match") {
      val en =
        <Node>
          <Node1/>
          <Node2/>
          <Node5/>
        </Node>
      val an =
        <Node>
          <Node0/>
          <Node1/>
          <Node2/>
          <Node3/>
          <Node4/>
          <Node5/>
        </Node>
      val result = run(en, an)
      assertResult(Match)(result)
    }
  }

  describe("when the first expected node is missing") {
    it("should return NodeNotFound with the missing first node label") {
      val en =
        <Node>
          <Node1/>
          <Node2/>
          <Node5/>
        </Node>
      val an =
        <Node>
          <Node0/>
          <Node2/>
          <Node5/>
        </Node>
      val result = run(en, an)
      assertResult(NodeNotFound("Node1"))(result)
    }
  }

  describe("when the last expected node is missing") {
    it("should return NodeNotFound with the missing last node label") {
      val en =
        <Node>
          <Node1/>
          <Node2/>
          <Node5/>
        </Node>
      val an =
        <Node>
          <Node1/>
          <Node2/>
        </Node>
      val result = run(en, an)
      assertResult(NodeNotFound("Node\\Node5"))(result)
    }
  }

  private def run(en: Node, an: Node): VerificationResult = {
    ChildVerifier(mv)(en.trim, an.trim)(using VerificationContext())
  }
}
