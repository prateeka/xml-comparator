package com.prateek.xmlcompare

import org.rogach.scallop.stringConverter
import org.scalatest.funspec.AnyFunSpec

class CommandLineParserSpec extends AnyFunSpec {
  describe("command line arguments are correctly read") {
    it("-a and -e are read") {
      val a1 = "file1"
      val a2 = "file2"
      val a3 = "file3"
      val args = s"-a $a2 -e $a1 -c $a3".split("\\s+")
      val clp = CommandLineParser[String](args)
      assertResult(clp.expected())(a1)
      assertResult(clp.actual())(a2)
    }

    it("--actual and --expected are read") {
      val a1 = "file1"
      val a2 = "file2"
      val a3 = "file3"
      val args = s"--actual $a2 --expected $a1 --config $a3".split("\\s+")
      val clp = CommandLineParser[String](args)
      assertResult(clp.expected())(a1)
      assertResult(clp.actual())(a2)
    }
  }
}
