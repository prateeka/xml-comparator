package com.prateek.xmlcompare.read

import scala.util.Try
import scala.xml.*

import java.io.{File, FileFilter}

import com.prateek.xmlcompare.verify.RootNodeSource

object XmlReader {

  private val logger = com.typesafe.scalalogging.Logger(getClass)

  /** Traverses the [[f:File]] directory to retrieve all the files in it and then reads each file to return its corresponding [[ComparisonNode]].
    * If [[f:File]] is only a file then reads it to return its corresponding [[ComparisonNode]].
    *
    * @param flr helps traverse the f:File
    * @param f   can be file or directory.
    * @return [[Seq]] If f is a file then only a single [[RootNodeSource]] is returned else [[Seq[RootNodeSource]]]
    */
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

  // top xml nodes to be compared
  trait ComparisonNode

  object DiscoverResponse extends ComparisonNode {
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
