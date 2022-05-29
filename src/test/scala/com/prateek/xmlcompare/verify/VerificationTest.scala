package com.prateek.xmlcompare.verify

import scala.util.Try
import scala.xml.{Node, NodeSeq}

import java.io.File

import org.scalatest.funspec.AnyFunSpec

class VerificationTest extends AnyFunSpec {

  given stringToFile: Conversion[String, File] = new File(_)

  given tuple2ToRootNodeSource: Conversion[(String, Node), RootNodeSource] =
    t =>
      RootNodeSource(
        t._1,
        Try {
          t._2
        }
      )

  describe("test Verifier.apply") {
    it("file matches itself only") {
      val ers1: RootNodeSource = ("f1", <n1/>)
      val ars1: RootNodeSource = ("f1", <n1/>)
      val ers2: RootNodeSource = ("f2", <n2/>)
      val ars2: RootNodeSource = ("f2", <n2/>)
      val ers = Seq(ers1, ers2)
      val ars = Seq(ars1, ars2)
      val vp: VerificationProvider = _ => Seq(LabelVerification)
      val svr = Verification(ers, ars, vp)
      assert(svr.contains(FileVerificationResult("f1", "f1", Match)))
      assert(svr.contains(FileVerificationResult("f1", "f1", Match)))
      assert(
        svr.contains(FileVerificationResult("f2", "f1", NodeNotFound("")))
      )
      assert(
        svr.contains(FileVerificationResult("f2", "f2", Match))
      )
    }
  }
}
