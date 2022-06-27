package com.prateek.xmlcompare.verify

import scala.util.Try
import scala.xml.{Node, NodeSeq}

import java.io.File

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*

import com.prateek.xmlcompare.read.{trim, DiscoverResponse, Message, Valid}
import com.prateek.xmlcompare.stringToFile
import com.prateek.xmlcompare.verify.XPathFactory.XPath

class VerifierSpec extends AnyFunSpec {

  private val vp: VerificationPredicate = (_: Message, _: VerifierId, _: XPath) => true

  given tuple2ToValid: Conversion[(String, Node), Valid] = { case (str: String, nd: Node) =>
    Valid(nd, str, DiscoverResponse)
  }

  describe("a set of expected files and a set of actual files being compared") {
    describe(
      "file1: an expected file matches an actual file"
    ) {
      describe(
        "file2: only the root node of an expected file and its corresponding actual file match while the child nodes DO NOT match"
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
            val vrs = Verifier(evs, avs, NodeVerifier(Seq(LabelVerifier(vp))))
            vrs should contain theSameElementsInOrderAs Seq(
              FVR("e1", "a1", Match),
              FVR("e2", "a2", Match)
            )
          }
        }

        describe("using Label, Attribute and Child verification") {
          it(
            "one expected file finds a match and the second expected file finds a mismatch with the correct missing node"
          ) {
            val ev1: Valid = (
              "e1",
              <n1>
                <n2>node 2</n2>
                <n3>
                  <n4>node 4</n4>
                  <n6>node 61</n6>
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
                  <n4>node 4</n4>
                  <n6>node 6</n6>
                </n3>
              </n1>
            )
            val evs: Seq[Valid] = trimNode(ev1, ev2)
            val avs: Seq[Valid] = trimNode(av1, av2)

            val vrs = {
              lazy val verifiers: Seq[Verifier] =
                Seq(LabelVerifier(vp), TextVerifier(vp), AttributeVerifier(vp), cv)
              lazy val nv: NodeVerifier = NodeVerifier(verifiers)
              lazy val cv: ChildVerifier = ChildVerifier(nv, vp)
              Verifier(evs, avs, nv)
            }

            vrs should contain theSameElementsInOrderAs Seq(
              FVR(
                "e1",
                "a2",
                NodeTextNotFound("n1\\n3\\n6\\%TEXT")
              ),
              FVR("e2", "a1", Match)
            )
          }
        }
      }

      describe(
        "file2: only the nodes of an expected file and its corresponding actual file match while the attibutes DO NOT match"
      ) {
        describe("using Label, Attribute and Child verification") {
          it(
            "one expected file finds a match and the second expected file finds a attibute mismatch with the correct attribute node"
          ) {
            val ev1: Valid = (
              "e1",
              <n1 a1="1" a11="11">
                <n2 a2="2" a22="22">node 2</n2>
                <n4 a4="4" a44="44">node 4</n4>
                <n5 a5="5" a55="55">node 5</n5>
              </n1>
            )
            val av1: Valid = (
              "a1",
              <n1 a1="1" a11="11">
                <n2 a2="2" a22="22">node 2</n2>
                <n4 a4="4" a44="44">node 4</n4>
                <n5 a6="6" a66="66">node 5</n5>
              </n1>
            )
            val ev2: Valid = (
              "e2",
              <n1 a1="1" a11="11">
                <n2 a2="2" a22="22">node 2</n2>
                <n4 a4="4" a44="44">node 4</n4>
                <n5 a6="6" a66="66">node 5</n5>
              </n1>
            )
            val av2: Valid = (
              "a2",
              <n1 a1="1" a11="11">
                <n2 a2="2" a22="22">node 2</n2>
                <n4 a4="4" a44="44">node 4</n4>
                <n5 a5="5" a66="66">node 5</n5>
              </n1>
            )
            val evs: Seq[Valid] = trimNode(ev1, ev2)
            val avs: Seq[Valid] = trimNode(av1, av2)

            val vs = {
              lazy val verifiers: Seq[Verifier] = Seq(LabelVerifier(vp), AttributeVerifier(vp), cv)
              lazy val nv: NodeVerifier = NodeVerifier(verifiers)
              lazy val cv: ChildVerifier = ChildVerifier(nv, vp)
              Verifier(evs, avs, nv)
            }

            vs should contain theSameElementsInOrderAs Seq(
              FVR(
                "e1",
                "a2",
                AttributeMissing("n1\\n5\\@a55")
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
}
