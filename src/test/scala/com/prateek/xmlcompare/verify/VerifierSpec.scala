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

  given seqToVerificationProvider
      : Conversion[Seq[Verifier], VerificationProvider] = vs =>
    (_: String) => vs

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
            "f1",
            <n1>
            <n2></n2>
          </n1>
          )
          val av1: Valid = (
            "f1",
            <n1>
            <n11></n11>
          </n1>
          )
          val ev2: Valid = (
            "f2",
            <n2>
            <n3></n3>
          </n2>
          )
          val av2: Valid = (
            "f2",
            <n2>
            <n22></n22>
          </n2>
          )
          val evs: Seq[Valid] = Seq(ev1, ev2)
          val avs: Seq[Valid] = Seq(av1, av2)
          val vp: VerificationProvider = Seq(LabelVerifier)
          val vrs = Verifier(evs, avs, vp)
          assert(vrs.contains(FileVerificationResult("f1", "f1", Match)))
          assert(vrs.contains(FileVerificationResult("f2", "f2", Match)))
          assert(vrs.sizeIs == 2)
        }
      }
    }
    {
      describe("using Label and Child verification") {
        it(
          "every expected file is correctly matched with a corresponding actual file"
        ) {
          val ev1: Valid = (
            "f1",
            <n1>
            <n2></n2>
          </n1>
          )
          val av1: Valid = (
            "f1",
            <n1>
            <n11></n11>
          </n1>
          )
          val ev2: Valid = (
            "f2",
            <n2>
            <n3></n3>
          </n2>
          )
          val av2: Valid = (
            "f2",
            <n2>
            <n22></n22>
          </n2>
          )
          val evs: Seq[Valid] = Seq(ev1, ev2)
          val avs: Seq[Valid] = Seq(av1, av2)
          val vp: VerificationProvider = Seq(LabelVerifier)
          val vrs = Verifier(evs, avs, vp)
          assert(vrs.contains(FileVerificationResult("f1", "f1", Match)))
          assert(vrs.contains(FileVerificationResult("f2", "f2", Match)))
          assert(vrs.sizeIs == 2)
          */
        }
      }
    }
  }

  private def getVerificationProvider(
      verifiers: Seq[Verifier]
  ): VerificationProvider = (_: String) => verifiers

}
