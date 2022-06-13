package com.prateek.xmlcompare.verify

import scala.util.Try
import scala.xml.{Node, NodeSeq}

import java.io.File

import org.scalatest.funspec.AnyFunSpec

import com.prateek.xmlcompare.read.{DiscoverResponse, Valid}
import com.prateek.xmlcompare.stringToFile
class VerifierSpec extends AnyFunSpec {

  given tuple2ToValid: Conversion[(String, Node), Valid] = { case (str, nd) =>
    Valid(nd, str, DiscoverResponse)
  }

  describe("a set of expected files and a set of actual files being compared") {
    describe(
      "only the root node of an expected file and its corresponding actual file match while the child nodes DO NOT match"
    ) {
      describe(
        "only using Label verification at root level and ignoring the child nodes"
      ) {
        it(
          "every expected file is correctly matched with a corresponding actual file"
        ) {
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
          assert(vrs.contains(FileVerificationResult("e1", "a1", Match)))
          assert(vrs.contains(FileVerificationResult("e2", "a2", Match)))
          assert(vrs.sizeIs == 2)
        }
      }
      describe("using Label and Child verification") {
        it(
          "no expected file finds a match and the correct max match is reported"
        ) {
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

          lazy val mvp: VerificationProvider = new MockVerificationProvider(cv)
          lazy val nv = NodeVerifier(mvp)
          lazy val cv = ChildVerifier(nv)
          val vrs = Verifier(evs, avs, nv)

          assert(vrs.sizeIs == 2)
          assert(
            vrs.contains(
              FileVerificationResult("e1", "a1", NodeNotFound("n1.n2"))
            )
          )
          assert(
            vrs.contains(
              FileVerificationResult("e2", "a2", NodeNotFound("n2.n3"))
            )
          )
        }
      }
    }
  }

  // this is needed to resolve the circular dependency between ChildVerifier and VerificationProvider
  class MockVerificationProvider(cv1: => ChildVerifier)
      extends VerificationProvider {
    lazy val cv: ChildVerifier = cv1

    override def apply(nt: String): Seq[Verifier] = {
      Seq(LabelVerifier, cv)
    }
  }

}
