package com.prateek.xmlcompare.config

import java.io.File
import java.net.URL

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*

import com.prateek.xmlcompare.config.VerifierId.*

class YamlReaderSpec extends AnyFunSpec:
  describe(
    "reading config/criteria-config.yaml that contains configuration for label, text and child"
  ) {
    it("VerificationConfig is correctly populated with the Seq[MessageVerificationConfig]") {
      val vc =
        val url: URL = getClass.getClassLoader.getResource("config/criteria-config.yaml")
        YamlReader(new File(url.toURI))

      vc.discoverResponse match
        case a :: b :: c :: Nil =>
          a should equal((raw"n1\\n2\\(.+\\)*n5", Set(Label, Text, Child)))
          b should equal((raw"e1\\e2\\e3\\%TEXT", Set(Ignore)))
          c should equal((raw"e1\\e2\\e3\\@a1", Set(Label, Text, Attribute)))
    }
  }
end YamlReaderSpec
