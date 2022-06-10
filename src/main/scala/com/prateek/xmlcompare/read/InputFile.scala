package com.prateek.xmlcompare.read

import scala.xml.Node

import java.io.File

trait InputFile

/** Stores the file and its corresponding xml node or a failure reading the file. If the file is valid then it is used for xml comparison.
  * The file is valid if it contains xml node like DiscoverRequest, DiscoverResponse else it is invalid.
  */
case class Valid(node: Node, file: File, message: Message) extends InputFile

case class Invalid(file: File) extends InputFile
