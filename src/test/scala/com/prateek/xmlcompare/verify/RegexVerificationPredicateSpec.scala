package com.prateek.xmlcompare.verify

import scala.collection.immutable.Set.*

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*

import com.prateek.xmlcompare.read.DiscoverResponse
import com.prateek.xmlcompare.verify.Identification.VerifierId

class RegexVerificationPredicateSpec extends AnyFunSpec:
  private val mvc2 = new MVC(".+", Set("labeltext", "child"))

  describe("test custom attribute selection") {
    it("attribute is marked for verification") {
      val mvc1 = new MVC(raw"e1\\(.+\\)*@a1", Set("attribute"))
      val vc = VerificationConfig(Seq(mvc1, mvc2))
      val bool = RegexVerificationPredicate(vc).apply(
        DiscoverResponse,
        "attribute",
        raw"e1\e2\e3\@a1"
      )
      bool shouldBe true
    }

    it("attribute is NOT marked for verification") {
      val mvc1 = new MVC(raw"e1\\(.+\\)*@a1", Set("ignore"))
      val vc = VerificationConfig(Seq(mvc1, mvc2))
      val bool = RegexVerificationPredicate(vc).apply(
        DiscoverResponse,
        "attribute",
        raw"e1\e2\@a1"
      )
      bool shouldBe false
    }
  }

  // Verifier 'attribute' listing below DOES NOT imply its semantic purpose but only added to have required number of verifiers for testing
  describe("test element") {
    describe("when xpath exact match is NOT found and only default matches") {
      it("verifier is selected for verification") {
        val mvc1 = new MVC(raw"e1\\e2\\e3", Set("labeltext", "attribute"))
        val vc = VerificationConfig(Seq(mvc1, mvc2))
        val bool = RegexVerificationPredicate(vc).apply(
          DiscoverResponse,
          "labeltext",
          raw"e1\e2"
        )
        bool shouldBe true
      }

      it("verifier is NOT selected for verification") {
        val mvc1 = new MVC(raw"e1\\e2\\e3", Set("labeltext", "attribute"))
        val vc = VerificationConfig(Seq(mvc1, mvc2))
        val bool = RegexVerificationPredicate(vc).apply(
          DiscoverResponse,
          "attribute",
          raw"e1\e2"
        )
        bool shouldBe false
      }
    }

    describe(
      "when xpath exact match is found in mulitple points and the smallest verifier list is chosen"
    ) {
      it("verifier is selected for verification") {
        val mvc1 = new MVC(raw"e1\\(.+\\)*e4", Set("attribute"))
        val vc = VerificationConfig(Seq(mvc1, mvc2))
        val bool1 = RegexVerificationPredicate(vc)(
          DiscoverResponse,
          "attribute",
          raw"e1\e2\e3\e4"
        )
        bool1 shouldBe true

        val bool2 = RegexVerificationPredicate(vc)(
          DiscoverResponse,
          "attribute",
          raw"e1\e4"
        )
        bool2 shouldBe true
      }

      it("verifier is NOT selected for verification") {
        val mvc1 = new MVC(raw"e1\\e2", Set("attribute"))
        val vc = VerificationConfig(Seq(mvc1, mvc2))
        val bool = RegexVerificationPredicate(vc)(
          DiscoverResponse,
          "child",
          raw"e1\e2"
        )
        bool shouldBe false
      }
    }

  }
