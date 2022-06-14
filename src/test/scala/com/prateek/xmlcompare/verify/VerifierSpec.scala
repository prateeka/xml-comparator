package com.prateek.xmlcompare.verify

import scala.util.Try
import scala.xml.{Node, NodeSeq}

import java.io.File

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*

import com.prateek.xmlcompare.read.{trim, DiscoverResponse, Valid}
import com.prateek.xmlcompare.stringToFile

class VerifierSpec extends AnyFunSpec {

  given tuple2ToValid: Conversion[(String, Node), Valid] = { case (str: String, nd: Node) =>
    Valid(nd, str, DiscoverResponse)
  }

  describe("a set of expected files and a set of actual files being compared") {
    describe(
      "an expected file matches an actual file"
    ) {
      describe(
        "only the root node of an expected file and its corresponding actual file match while the child nodes DO NOT match"
      ) {
        describe("only using Label verification at root level and ignoring the child nodes") {
          it("every expected file is correctly matched with a corresponding actual file") {
            val ev1: Valid = (
              "e1",
              <n1>
                <n2></n2>
              </n1>
            )
            val av1: Valid = (
              "a1",
              <n1>
                <n11></n11>
              </n1>
            )
            val ev2: Valid = (
              "e2",
              <n2>
                <n3></n3>
              </n2>
            )
            val av2: Valid = (
              "a2",
              <n2>
                <n22></n22>
              </n2>
            )
            val evs: Seq[Valid] = Seq(ev1, ev2)
            val avs: Seq[Valid] = Seq(av1, av2)
            val vp: VerificationProvider = (_: String) => Seq(LabelVerifier)
            val vrs = Verifier(evs, avs, NodeVerifier(vp))
            vrs should contain theSameElementsInOrderAs Seq(
              FVR("e1", "a1", Match),
              FVR("e2", "a2", Match)
            )
          }
        }
        describe("using Label and Child verification") {
          it(
            "one expected file finds a match and the second expected file finds a mismatch with the correct missing node"
          ) {
            val ev1: Valid = (
              "e1",
              <n1>
                <n2>node 2</n2>
                <n3>
                  <n4>node 4</n4>
                  <n5>node 5</n5>
                </n3>
              </n1>
            )
            val av1: Valid = (
              "a1",
              <n1>
                <n2>node 2</n2>
                <n3>
                  <n4>node 4</n4>
                  <n5>node 6</n5>
                </n3>
              </n1>
            )
            val ev2: Valid = (
              "e2",
              <n1>
                <n2>node 2</n2>
                <n3>
                  <n4>node 4</n4>
                  <n5>node 6</n5>
                </n3>
              </n1>
            )
            val av2: Valid = (
              "a2",
              <n1>
                <n2>node 2</n2>
                <n3>
                  <n41>node 4</n41>
                  <n5>node 6</n5>
                </n3>
              </n1>
            )
            val evs: Seq[Valid] = trimNode(ev1, ev2)
            val avs: Seq[Valid] = trimNode(av1, av2)

            val vrs = {
              lazy val mvp: VerificationProvider = new MockVerificationProvider(cv)
              lazy val nv = NodeVerifier(mvp)
              lazy val cv = ChildVerifier(nv)
              Verifier(evs, avs, nv)
            }

            vrs should contain theSameElementsInOrderAs Seq(
              FVR(
                "e1",
                "a1",
                NodeTextNotFound("n1.n3.n5.node 5")
              ),
              FVR("e2", "a1", Match)
            )
          }
        }
      }
    }
  }

  private def trimNode(v1: Valid, v2: Valid) = Seq(v1, v2).map({ case v @ Valid(n, _, _) =>
    v.copy(node = n.trim)
  })

  // this is needed to resolve the circular dependency between ChildVerifier and VerificationProvider
  class MockVerificationProvider(cv1: => ChildVerifier) extends VerificationProvider {
    lazy val cv: ChildVerifier = cv1

    override def apply(nt: String): Seq[Verifier] = Seq(LabelVerifier, cv)
  }

}
