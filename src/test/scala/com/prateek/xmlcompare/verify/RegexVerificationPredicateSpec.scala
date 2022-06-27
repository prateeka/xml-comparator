package com.prateek.xmlcompare.verify

import scala.collection.immutable.Set.*

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*

import com.prateek.xmlcompare.config.{MVC, VerificationConfig, VerifierId, XPathRegex}
import com.prateek.xmlcompare.config.VerifierId.*
import com.prateek.xmlcompare.read.DiscoverResponse
import com.prateek.xmlcompare.verify.XPathFactory.XPath

class RegexVerificationPredicateSpec extends AnyFunSpec:
  private val mvc1 = new MVC(".+", Set(Label, Text, Child))

  private def run(vc: VerificationConfig, testVerifierId: VerifierId, xp: XPath) =
    RegexVerificationPredicate(vc).apply(
      DiscoverResponse,
      testVerifierId,
      xp
    )

  private def verificationConfig(mvc: MVC) = VerificationConfig(Seq(mvc1, mvc))

  private def mvc(xpr: XPathRegex, vid: Set[VerifierId]) = new MVC(xpr, vid)

  describe("test TEXT selection") {
    val xpr: XPathRegex = raw"e1\\e2\\(.+\\)%TEXT"
    val testVerifierId = Text
    it("node TEXT is marked for verification") {
      val vc = verificationConfig(mvc(xpr, Set(Text)))
      val bool = run(vc, testVerifierId, raw"e1\e2\e3\%TEXT")
      bool shouldBe true
    }

    it("node TEXT is explicitly marked for Ignore") {
      val vc = verificationConfig(mvc(xpr, Set(Ignore)))
      val bool = run(vc, testVerifierId, raw"e1\e2\e3\%TEXT")
      bool shouldBe false
    }
  }

  describe("test custom attribute selection") {
    val xpr: XPathRegex = raw"e1\\(.+\\)*@a1"
    val testVerifierId = Attribute
    it("attribute is marked for verification") {
      val vc = verificationConfig(mvc(xpr, Set(Attribute)))
      val bool = run(vc, testVerifierId, raw"e1\e2\e3\@a1")
      bool shouldBe true
    }

    it("attribute is explicitly marked for Ignore") {
      val vc = verificationConfig(mvc(xpr, Set(Ignore)))
      val bool = run(vc, testVerifierId, raw"e1\e2\@a1")
      bool shouldBe false
    }
  }

  // Verifier 'attribute' listing below DOES NOT imply its semantic purpose but only added to have required number of verifiers for testing
  describe("test element") {
    describe("when xpath exact match is NOT found and only default matches") {
      // dummy: only to increase the config
      val xpr: XPathRegex = raw"e1\\e2\\e3"

      it("verifier is selected for verification") {
        val vc = verificationConfig(mvc(xpr, Set(Label, Child)))
        val bool = run(vc, Label, raw"e1\e2")
        bool shouldBe true
      }

      it("verifier is NOT selected for verification") {
        val vc = verificationConfig(mvc(xpr, Set(Label, Child)))
        val bool = run(vc, Attribute, raw"e1\e2")
        bool shouldBe false
      }
    }

    describe(
      "when xpath exact match is found in multiple points and the smallest verifier list is chosen"
    ) {
      val xpr: XPathRegex = raw"e1\\(.+\\)*e4"
      val testVerifierId = Attribute

      it("verifier is selected for verification") {
        val vc = verificationConfig(mvc(xpr, Set(Attribute)))
        val bool1 = run(vc, Attribute, raw"e1\e2\e3\e4")
        bool1 shouldBe true

        val bool2 = run(vc, Attribute, raw"e1\e4")
        bool2 shouldBe true
      }

      it("verifier is NOT selected for verification") {
        val vc = verificationConfig(mvc(xpr, Set(Attribute)))
        val bool1 = run(vc, Child, raw"e1\e2\e3\e4")
        bool1 shouldBe false
      }
    }

  }
