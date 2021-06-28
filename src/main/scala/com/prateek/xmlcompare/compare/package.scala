package com.prateek.xmlcompare

/*

import scala.language.implicitConversions

import scala.xml.{ Elem, Node, Text }

package object compare {

  implicit def nodeToString(n: xml.Node): String = n match {
    case Text(f) => f
    case Elem(_, f, _, _, _*) => f
  }

  /** checks if a match operation yields [[NodeFound]] for all the elements of a
 * [[Seq]]. If [[NodeNotFound]] is ever found then the match is terminated
 * (hence this match is lazy) and [[NodeNotFound]] returned.
 *
 * @param seq
 *   match operations are executed against all the elements of [[Seq]]
 * @param fn
 *   match operation
 * @return
 *   [[NodeFound]] if match operation succeeds for all elements of [[Seq]]
 *   else [[NodeNotFound]]
 */
  def lazyAllMatch[T](
      seq: Seq[T],
      fn: T => ComparatorResult
  ): ComparatorResult = {
    val cr = seq.iterator
      .map(fn)
      .find({
        case _: NodeNotFound => true
        case NodeFound => false
      })
      .getOrElse(NodeFound)
    cr
  }

  object NodeToString {
    def unapply(tuple: (Node, Node)): Option[(String, String)] = {
      tuple match {
        case (Text(f), Text(s)) => Option((f, s))
        case (Elem(_, f, _, _, _*), Elem(_, s, _, _, _*)) => Option(f, s)
      }
    }
  }
}
 */
