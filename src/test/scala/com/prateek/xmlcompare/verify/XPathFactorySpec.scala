package com.prateek.xmlcompare.verify

import scala.xml.{Elem, MetaData, Node, UnprefixedAttribute}

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*

import com.prateek.xmlcompare.verify.XPathFactory.{appendAttributeKey, XPath}

class XPathFactorySpec extends AnyFunSpec {
  describe("xpath for Elem") {
    describe("when VerificationContext is empty") {
      it("xpath is empty") {
        XPathFactory(Nil: Seq[Node]) shouldBe empty
      }
    }

    describe("when VerificationContext is non-empty") {
      it("xpath is non-empty") {
        val n: Node =
          <n1>
            <n2></n2>
          </n1>
        val vc = Seq((n \\ "n1").head, (n \\ "n2").head)
        XPathFactory(vc) shouldBe "n1\\n2"
      }
    }
  }
  describe("attach an attribute key to a parent node") {
    it("returns correct xpath for attribute key") {
      val appendedXpath = "e1\\e2".appendAttributeKey("k1")
      appendedXpath shouldBe "e1\\e2\\@k1"
    }
  }
}
