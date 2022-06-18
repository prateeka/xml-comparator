package com.prateek.xmlcompare.verify

import scala.xml.Node

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*

class XPathFormatterSpec extends AnyFunSpec {
  describe("xpath for Elem") {
    describe("when VerificationContext is empty") {
      it("returns empty string") {
        XPathFormatter(Nil: Seq[Node]) shouldBe empty
      }
    }

    describe("when VerificationContext is non-empty") {
      it("returns correct xpath") {
        val n: Node = <n1>
          <n2></n2>
        </n1>
        val vc = Seq((n \\ "n1").head, (n \\ "n2").head)
        XPathFormatter(vc) shouldBe "n1\\n2"
      }
    }
  }
  describe("attach an attribute key to a parent node") {
    it("returns correct xpath for attribute key") {
      val nxp = "n1\\n2"
      XPathFormatter.attribute(nxp, "k1") shouldBe "n1\\n2\\@k1"
    }
  }
}
