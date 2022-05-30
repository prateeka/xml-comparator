package com.prateek.xmlcompare.verify

import scala.util.Try
import scala.xml.{Node, NodeSeq}

import java.io.File

import org.scalatest.funspec.AnyFunSpec

class VerifierSpec extends AnyFunSpec {

  given stringToFile: Conversion[String, File] = new File(_)

  given tuple2ToRootNodeSource: Conversion[(String, Node), RootNodeSource] = {
    case (str, nd) => RootNodeSource(str, Try(nd))
  }

  it("file matches itself only") {
    val erns1: RootNodeSource = ("f1", <n1/>)
    val arns1: RootNodeSource = ("f1", <n1/>)
    val erns2: RootNodeSource = ("f2", <n2/>)
    val arns2: RootNodeSource = ("f2", <n2/>)
    val ernss: Seq[RootNodeSource] = Seq(erns1, erns2)
    val arnss: Seq[RootNodeSource] = Seq(arns1, arns2)
    val vrs = Verifier(ernss, arnss, LabelVerifier)
    assert(vrs.contains(FileVerificationResult("f1", "f1", Match)))
    assert(vrs.contains(FileVerificationResult("f2", "f2", Match)))
    assert(vrs.sizeIs == 2)
  }
}
