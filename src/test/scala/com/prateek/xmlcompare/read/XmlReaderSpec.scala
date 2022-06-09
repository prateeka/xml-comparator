package com.prateek.xmlcompare.read

import scala.util.{Failure, Success, Try}
import scala.xml.{Node, NodeSeq}

import java.io.File

import org.scalatest.funspec.AnyFunSpec

import com.prateek.xmlcompare.read.{FileListReader, XmlReader}
import com.prateek.xmlcompare.stringToFile
import com.prateek.xmlcompare.verify.{InputFile, Invalid}

class XmlReaderSpec extends AnyFunSpec {

  /*  def nodeLabel(rns: Seq[InputFile]): String = {
    object SingletonNode {
      def unapply(ns: Node): Option[Node] = {
        ns match {
          case Success(n) if n.size == 1 => Option(n.head)
          case _                         => None
        }
      }
    }

    rns match {
      case Valid(SingletonNode(n), _) :: Nil => n.label
      case _                                 => throw new Exception("unhandled")
    }
  }*/

  it("non xml file input returns empty Seq") {
    val node = inputFileReader(_ => Seq.empty, "empty.txt")
    assertResult(Seq.empty)(node)
  }

  it("throw exception for non matching xml") {
    import scala.collection.immutable.{AbstractSeq, LinearSeq}

    val node = inputFileReader(f => Seq(f), "invalidDiscoverResponse.xml")
    assertResult(Seq(Invalid(toFile("invalidDiscoverResponse.xml"))))(node)
  }

  /*
  it("read valid DISCOVER response") {
    val node = inputFileReader(f => Seq(f), "validDiscoverResponse.xml")
    assertResult("DiscoverResponse")(nodeLabel(node))
  }
   */

  private def inputFileReader(
      flr: FileListReader,
      s: String
  ): Seq[InputFile] = {
    val f: File = toFile(s)
    XmlReader(flr)(f)
  }

  private def toFile(s: String) = {
    val fileUrl = getClass.getResource(s)
    new File(fileUrl.toURI)
  }
}
