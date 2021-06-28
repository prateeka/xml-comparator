package com.prateek.xmlcompare.read

import scala.util.Try
import scala.xml.*

import java.io.{File, FileFilter}

import com.prateek.xmlcompare.verify.RootNodeSource

/** Validates f: [[File]] passed exists and accordingly returns:
  *   1. If "f" is a directory, list of xml files inside "f". 2. Else if "f" is
  *      xml file then a singleton Seq(f). 3. Else, return an empty sequence.
  */
trait FileListReader extends (File => Seq[File])

object FileListReader {
  val default: FileListReader = (f: File) => {
    assert(f.exists(), s"$f should refer to an existing file or directory")
    val xmlFiles: Seq[File] = if (f.isDirectory) {
      val matchedFiles = f.listFiles(_.getName.isXmlType)
      matchedFiles.toList
    } else if (f.getName.isXmlType)
      Seq(f)
    else
      Seq.empty
    xmlFiles
  }

  extension (fn: String) {
    def isXmlType: Boolean = {
      fn.matches(".+-req\\.xml")
    }
  }
}

object XmlReader {

  private val logger = com.typesafe.scalalogging.Logger(getClass)

  def apply(flr: FileListReader, f: File): Seq[RootNodeSource] = {
    val fs = flr(f)
    fs.map(f1 => {
      val doc = XML.loadFile(f1)
      val trimmedNode = Utility.trim(doc)
      trimmedNode match {
        case DiscoverResponse(n) if n.size == 1 =>
          logger.debug(s"matched DiscoverResponse is $n")
          RootNodeSource(f, Try(n))
        case _ =>
          RootNodeSource(
            f,
            Try(
              throw XmlNodeParsingException("no match")
            )
          )
      }
    })
  }

  object DiscoverResponse {
    def unapply(node: Node): Option[Node] = {
      val ns = node \\ "Body" \ "DiscoverResponse"
      ns.toList match {
        case h :: Nil => Some(h)
        case _        => None
      }
    }
  }
}

case class XmlNodeParsingException(msg: String) extends Exception(msg)
