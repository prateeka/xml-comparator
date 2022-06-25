package com.prateek.xmlcompare.verify

import scala.collection.immutable.Set.*

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*

import com.prateek.xmlcompare.read.DiscoverResponse
import com.prateek.xmlcompare.verify.Identification.VerifierId

class RegexVerificationPredicateSpec extends AnyFunSpec:
  describe("test node label") {
    describe("when xpath exact match is NOT found and only default matches") {
      it("is true as the verifier is in default verifiers list") {
        val mvc1 = new MVC(raw"e1\\e2\\e3", Set("labeltext", "attribute"))
        val mvc2 = new MVC(".+", Set("labeltext", "child"))
        val vc = VerificationConfig(Seq(mvc1, mvc2))
        val bool = RegexVerificationPredicate(vc).apply(
          DiscoverResponse,
          "labeltext",
          raw"e1\e2"
        )
        bool shouldBe true
      }

      it("is false as the verifier is NOT in default verifiers list") {
        val mvc1 = new MVC(raw"e1\\e2\\e3", Set("labeltext", "attribute"))
        val mvc2 = new MVC(".+", Set("labeltext", "child"))
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
      it("is true as the verifier is in default verifiers list") {
        val mvc1 =
          new MVC(raw"e1\\e2", Set("attribute"))
        val mvc2 = new MVC(".+", Set("labeltext", "child"))
        val vc = VerificationConfig(Seq(mvc1, mvc2))
        val bool = RegexVerificationPredicate(vc)(
          DiscoverResponse,
          "attribute",
          raw"e1\e2"
        )
        bool shouldBe true
      }

      it("is false as the verifier is NOT in default verifiers list") {
        val mvc1 = new MVC(raw"e1\\e2", Set("attribute"))
        val mvc2 = new MVC(".+", Set("labeltext", "child"))
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
