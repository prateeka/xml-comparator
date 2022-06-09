package com.prateek.xmlcompare

import scala.xml.{Node, NodeSeq}

trait RequestResponseType {
  def comparingNode(n: Node): Option[Node] = {
    val ns = n \\ "Body" \ "DiscoverResponse"
    ns.toList match {
      case h :: Nil => Some(h)
      case _        => None
    }
  }
}

trait RequestType extends RequestResponseType

trait ResponseType extends RequestResponseType

/*
case class DiscoverResponse() extends ResponseType {
  override def comparingNode(n: Node): Option[NodeSeq] = {
    ???
  }
}
 */
