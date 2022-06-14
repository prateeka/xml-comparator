package com.prateek.xmlcompare.compare

import java.io.File

sealed trait ComparatorResult {}
case class Subset(first: File, second: File) extends ComparatorResult

//TODO: make reason a stack of nodes and add reason for failing
case class ResponseFailure(first: File, second: File, reason: String) extends ComparatorResult

case class FileNotFound(ff: File, sf: File, nnf: NodeNotFound) extends ComparatorResult

case class NodeNotFound(node: String, cc: String) extends ComparatorResult
case object NodeFound extends ComparatorResult
