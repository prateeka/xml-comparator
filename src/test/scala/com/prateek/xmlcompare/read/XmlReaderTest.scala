package com.prateek.xmlcompare.read

import scala.util.{Failure, Success, Try}
import scala.xml.{Node, NodeSeq}

import java.io.File

import org.scalatest.funspec.AnyFunSpec

import com.prateek.xmlcompare.read.{
  FileListReader,
  XmlNodeParsingException,
  XmlReader
}
import com.prateek.xmlcompare.verify.RootNodeSource

class XmlReaderTest extends AnyFunSpec {

  def nodeLabel(rns: Seq[RootNodeSource]): String = {
    object SingletonNode {
      def unapply(ns: Try[NodeSeq]): Option[Node] = {
        ns match {
          case Success(n) if n.size == 1 => Option(n.head)
          case _                         => None
        }
      }
    }

    rns match {
      case RootNodeSource(_, SingletonNode(n)) :: Nil   => n.label
      case RootNodeSource(_, Failure(exception)) :: Nil => throw exception
      case _ => throw new Exception("unhandled")
    }
  }

  describe("test for xml read") {
    it("non xml file input returns empty Seq") {
      val node = nodeSeqReader(_ => Seq.empty, "empty.txt")
      assertResult(Seq.empty)(node)
    }

    it("throw exception for non matching xml") {
      val node = nodeSeqReader(f => Seq(f), "invalidDiscoverResponse.xml")
      assertThrows[XmlNodeParsingException](nodeLabel(node))
    }

    it("read valid DISCOVER response") {
      val node = nodeSeqReader(f => Seq(f), "validDiscoverResponse.xml")
      assertResult("DiscoverResponse")(nodeLabel(node))
    }
  }

  private def nodeSeqReader(
      flr: FileListReader,
      fn: String
  ): Seq[RootNodeSource] = {
    val f = {
      val fileUrl = getClass.getResource(fn)
      new File(fileUrl.toURI)
    }
    XmlReader(flr)(f)
  }
}
