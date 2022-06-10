package com.prateek.xmlcompare.read

import scala.util.{Failure, Success, Try}
import scala.xml.{Node, NodeSeq}

import java.io.File

import org.scalatest.funspec.AnyFunSpec

import com.prateek.xmlcompare.read.{FileListReader, InputFileReader}
import com.prateek.xmlcompare.stringToFile

class InputFileReaderSpec extends AnyFunSpec {

  describe("read invalid files") {
    describe("input is a non-xml file") {
      it("returns empty Seq") {
        val node = inputFileReader(_ => Seq.empty, "empty.txt")
        assertResult(Seq.empty)(node)
      }
    }

    describe(
      "input is an xml file but does not follow any specified Message format"
    ) {
      it("returns Invalid file containing the name of the file") {
        val node = inputFileReader(f => Seq(f), "invalidDiscoverResponse.xml")
        assertResult(Seq(Invalid(toFile("invalidDiscoverResponse.xml"))))(node)
      }
    }
  }

  describe("read valid file") {
    it("returns Valid with correct comparison node and the xml file name") {
      val inputFileName = "validDiscoverResponse.xml"
      val node = inputFileReader(f => Seq(f), inputFileName)
      node match {
        case Valid(an, af, _) :: Nil =>
          assertResult(an.label)("root")
          assertResult(toFile(inputFileName))(af)
      }
    }
  }

  private def inputFileReader(
      flr: FileListReader,
      s: String
  ): Seq[InputFile] = {
    val f: File = toFile(s)
    InputFileReader(flr)(f)
  }

  private def toFile(s: String) = {
    val fileUrl = getClass.getResource(s)
    new File(fileUrl.toURI)
  }
}
