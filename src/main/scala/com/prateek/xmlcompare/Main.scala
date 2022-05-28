package com.prateek.xmlcompare

import scala.util.{Failure, Success, Try}

import java.io.File

import org.rogach.scallop.fileConverter

import com.prateek.xmlcompare.read.{FileListReader, XmlReader}
import com.prateek.xmlcompare.verify.{RootNodeSource, Verification}

object Main extends App {

  private val logger = com.typesafe.scalalogging.Logger(getClass)

  @main
  def execute(args: String*): Unit = {
    import com.prateek.xmlcompare.verify.LabelVerification
    logger.debug(s"args: $args")
    val clp = CommandLineParser[File](args)
    //    TODO: add one more flag to indicate if the input refer to files or directories
    val (exp, act) = (clp.expected(), clp.actual())
    logger.info(s"expected: $exp actual: ${clp.actual}")
    val xmlReaderFunction = XmlReader(FileListReader.default)
    val ef = xmlReaderFunction(exp)
    val af = xmlReaderFunction(act)

    Verification(ef.filterSuccess, af.filterSuccess)
  }

  extension (s: Seq[RootNodeSource]) {
    // Returns only [[RootNodeSource]] that encloses a successfully parsed file while the failed instances are logged and dropped
    def filterSuccess: Seq[RootNodeSource] = {
      s.filter({
        case RootNodeSource(f, Failure(ex)) =>
          logger.warn(s"file $f failed with: $ex")
          false
        case rns => true
      })
    }
  }
}
