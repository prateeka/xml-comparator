package com.prateek.xmlcompare.read

import scala.util.Try
import scala.xml.*

import java.io.{File, FileFilter}

import com.prateek.xmlcompare.verify.RootNodeSource

object XmlReader {

  private val logger = com.typesafe.scalalogging.Logger(getClass)

  def apply(flr: FileListReader)(f: File): Seq[RootNodeSource] = {
    val fs: Seq[File] = flr(f)
    fs.map(f1 => {
      val doc: Elem = XML.loadFile(f1)
      val trimmedNode: Node = Utility.trim(doc)
      trimmedNode match {
        case DiscoverResponse(n) if n.size == 1 =>
          logger.debug(s"matched DiscoverResponse is $n")
          RootNodeSource(f, Try(n))
        case _ =>
          RootNodeSource(
            f,
            Try {
              throw XmlNodeParsingException("no match")
            }
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
