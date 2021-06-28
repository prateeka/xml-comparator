package com.prateek.xmlcompare.compare

/*

import scala.language.implicitConversions

import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.xml.Node

import java.io.File

import com.prateek.xmlcompare.yaml.{ComparingCriteriaYamlReader, Criteria}

case class ComparingCriteriaProvider(f: File) {
  private val config = ComparingCriteriaYamlReader(f)
/*
  private val pf: Criteria => PartialFunction[String, Seq[String]] =
    (cr: Criteria) => {
      //      case tl if cr.exclude.contains(tl) => Seq.empty
      case tl if cr.nodeConfig.containsKey(tl) =>
        //        cr.nodeConfig.get(tl).asScala
        ???
      case _                                   =>
        //        cr.defaultInclude.asScala
        ???
    }
 */

  def reqCriteria(tl: String): Iterable[String] = {
    //    pf(config.request)(tl)
    ???
  }
}

trait ComparingCriteria {
  def apply(fn: Node, sn: Node)(implicit ctx: Context): ComparatorResult

  implicit def comparingCriteriaName(cc: ComparingCriteria): String = {
    cc.getClass.getSimpleName
  }
}

object ComparingCriteria {
  def apply(fn: Node): Seq[ComparingCriteria] = {
    Seq(TextLabel, ChildCount, RecursiveMatch)
  }
}

object ChildCount extends ComparingCriteria {
  override def apply(fn: Node, sn: Node)(implicit
    ctx: Context
  ): ComparatorResult = {
    if (fn.child.length == sn.child.length) NodeFound
    else NodeNotFound(fn, this)
  }
}

object TextLabel extends ComparingCriteria {
  override def apply(fn: Node, sn: Node)(implicit
    ctx: Context
  ): ComparatorResult = {
    val bool = (fn, sn) match {
      case NodeToString(f, s) => f.equalsIgnoreCase(s)
    }
    if (bool) NodeFound else NodeNotFound(fn, this)
  }
}

// Match the nodes recursively
object RecursiveMatch extends ComparingCriteria {
  override def apply(fn: Node, sn: Node)(implicit
    ctx: Context
  ): ComparatorResult = {

    /** Matches f with all [[sn.child]] for ANY [[NodeFound]]. If no match
 * occurs, then returns the [[NodeNotFound]] with the longest
 * [[NodeNotFound.node]] as it is the one which has matched the deepest.
 *
 * @param f
 * one of [[fn.child]]
 * @return
 * [[NodeFound]] for match else [[NodeNotFound]] for the deepest match
 */
    def anyMatches(f: Node): ComparatorResult = {
      val llm = sn.child
        .to(LazyList)
        .map(s => Comparator(f, s))

      llm
        .find({
          case NodeFound       => true
          case _: NodeNotFound => false
        })
        .getOrElse(llm.reduce((c1: ComparatorResult, c2: ComparatorResult) => {
          (c1, c2) match {
            case (NodeNotFound(n1, _), NodeNotFound(n2, _))
              if n1.length >= n2.length =>
              c1
            case (NodeNotFound(n1, _), NodeNotFound(n2, _))
              if n1.length < n2.length  =>
              c2
          }
        }))
    }

    val childMatch = lazyAllMatch(fn.child, anyMatches)
    // prepending parent node text/label to child's for getting the exact depth of the child
    childMatch match {
      case nf@NodeFound           => nf
      case nnf@NodeNotFound(n, _) =>
        nnf.copy(node = s"${nodeToString(fn)}.$n")
    }
  }
}

case class Context(f1: File, f2: File) {
  val st: StackWrapper = new StackWrapper
}
 */
