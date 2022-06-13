package com.prateek.xmlcompare.read

import scala.util.Try
import scala.xml.*

import java.io.{File, FileFilter}

object InputFileReader {

  private val logger = com.typesafe.scalalogging.Logger(getClass)

  /** Traverses the [[f:File]] directory to retrieve all the files in it and then reads each file to return its corresponding [[ComparisonNode]].
    * If [[f:File]] is only a file then reads it to return its corresponding [[ComparisonNode]].
    *
    * @param flr helps traverse the f:File
    * @param f   can be file or directory.
    * @return [[Seq]] If f is a file then only a single [[InputFile]] is returned else [[Seq[InputFile]]]
    */
  def apply(flr: FileListReader)(f: File): Seq[InputFile] = {
    val fs: Seq[File] = flr(f)
    fs.map(f1 => {
      val doc: Elem = XML.loadFile(f1)
      logger.info(s"xml loaded from $f1 is $doc")
      val trimmedNode: Node = Utility.trim(doc)
      trimmedNode match {
        case DiscoverResponse.Applied((n, m)) =>
          logger.debug(s"matched DiscoverResponse is $n")
          Valid(n, f1, m)
        case _ =>
          Invalid(f1)
      }
    })
  }
}
