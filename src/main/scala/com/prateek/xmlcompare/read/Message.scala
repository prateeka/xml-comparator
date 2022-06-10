package com.prateek.xmlcompare.read

import scala.xml.{Node, NodeSeq}

// type of messages exchanged between Atscale and client
trait Message {
  t =>
  def comparisonNode(n: Node): NodeSeq

  object Applied {
    def unapply(node: Node): Option[(Node, Message)] = {
      comparisonNode(node).toList match {
        case h :: Nil => Some((h, t))
        case _        => None
      }
    }
  }
}

case object DiscoverResponse extends Message {
  override def comparisonNode(n: Node): NodeSeq =
    n \\ "Body" \ "DiscoverResponse" \ "return" \ "root"
}
