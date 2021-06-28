package com.prateek.xmlcompare.verify

import scala.xml.Node

import org.scalatest.funspec.AnyFunSpec

class ChildVerificationTest extends AnyFunSpec {

  describe("ChildVerification") {
    describe("when expected and actual nodes count do not match") {
      describe("when all the expected nodes match subset of actual nodes") {
        it("should return Match") {
          val en = <Node>
            <Node1/>
            <Node2/>
            <Node5/>
          </Node>
          val an = <Node>
            <Node0/>
            <Node1/>
            <Node2/>
            <Node3/>
            <Node4/>
            <Node5/>
          </Node>
          val mv = new Verification {
            override def apply(exp: Node, act: Node)(using
                ctx: Context
            ): VerificationResult = {
              if exp.equals(act) then Match
              else NodeNotFound(exp.label)
            }
          }
          val result = ChildVerification(mv)(en, an)(using Context())
          assertResult(Match)(result)
        }
      }

      describe("when the first expected node is missing") {
        it("should return NodeNotFound with the missing first node label") {
          val en = <Node>
            <Node1/>
            <Node2/>
            <Node5/>
          </Node>
          val an = <Node>
            <Node0/>
            <Node2/>
            <Node5/>
          </Node>
          val mv = new Verification {
            override def apply(exp: Node, act: Node)(using
                ctx: Context
            ): VerificationResult = {
              if exp.equals(act) then Match
              else NodeNotFound(exp.label)
            }
          }
          val result = ChildVerification(mv)(en, an)(using Context())
          assertResult(NodeNotFound("Node1"))(result)
        }
      }

      describe("when the last expected node is missing") {
        it("should return NodeNotFound with the missing last node label") {
          val en = <Node>
            <Node1/>
            <Node2/>
            <Node5/>
          </Node>
          val an = <Node>
            <Node1/>
            <Node2/>
          </Node>
          val mv = new Verification {
            override def apply(exp: Node, act: Node)(using
                ctx: Context
            ): VerificationResult = {
              if exp.equals(act) then Match
              else NodeNotFound(exp.label)
            }
          }
          val result = ChildVerification(mv)(en, an)(using Context())
          assertResult(NodeNotFound("Node5"))(result)
        }
      }
    }
  }
}
