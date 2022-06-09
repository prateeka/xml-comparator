package com.prateek.xmlcompare.verify

import scala.util.Try
import scala.xml.{Node, NodeSeq}

import java.io.File

import org.scalatest.funspec.AnyFunSpec

import com.prateek.xmlcompare.stringToFile

class VerifierSpec extends AnyFunSpec {

  given tuple2ToRootNodeSource: Conversion[(String, Node), Valid] = {
    case (str, nd) => Valid(nd, str)
  }

  it("file matches itself only") {
    val erns1: Valid = ("f1", <n1/>)
    val arns1: Valid = ("f1", <n1/>)
    val erns2: Valid = ("f2", <n2/>)
    val arns2: Valid = ("f2", <n2/>)
    val ernss: Seq[Valid] = Seq(erns1, erns2)
    val arnss: Seq[Valid] = Seq(arns1, arns2)
    val vrs = Verifier(ernss, arnss, LabelVerifier)
    assert(vrs.contains(FileVerificationResult("f1", "f1", Match)))
    assert(vrs.contains(FileVerificationResult("f2", "f2", Match)))
    assert(vrs.sizeIs == 2)
  }
}
