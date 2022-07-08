package com.prateek.xmlcompare

import scala.util.{Failure, Success, Try}

import java.io.File

import org.rogach.scallop.fileConverter

import com.prateek.xmlcompare.read.*
import com.prateek.xmlcompare.verify.*

object Main extends App {

  private val logger = com.typesafe.scalalogging.Logger(getClass)

  @main
  def execute(args: String*): Unit = {
    logger.debug(s"args: $args")
    val clp = CommandLineParser[File](args)
    //    TODO: add one more flag to indicate if the input refer to files or directories
    val (exp: File, act: File) = (clp.expected(), clp.actual())
    logger.info(s"expected: $exp actual: ${clp.actual}")
    val ipFileReaderFunction = InputFileReader(FileListReader.default)
    val ef: Seq[InputFile] = ipFileReaderFunction(exp)
    val af: Seq[InputFile] = ipFileReaderFunction(act)

    val efp = ef.partitionn
    val afp = af.partitionn
    Verifier(efp._1, afp._1)
  }

  extension (s: Seq[InputFile]) {

    /** Partitions the input Seq[RootNodeContext] into valid and invalid [[Valid]]
      *
      * @return a tuple with tuple._1 containing valid files and tuple._2 containing invalid files
      */
    def partitionn: (Seq[Valid], Seq[Invalid]) = {
      val t = s.partitionMap({
        case r: Valid   => Left(r)
        case r: Invalid => Right(r)
      })
      t
    }
  }
}
