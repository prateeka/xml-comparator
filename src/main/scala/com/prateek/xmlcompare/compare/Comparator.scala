package com.prateek.xmlcompare.compare
/*

import scala.xml.Node

object Comparator {
  private val logger = com.typesafe.scalalogging.Logger(getClass)

  // matching request files from one directory with request files from another
  def apply(
      fnt: Seq[FileNodeTuple],
      snt: Seq[FileNodeTuple]
  ): Seq[ComparatorResult] = {
    val result = fnt.map({ case FileNodeTuple(ff, fn) =>
      val llm: Seq[ComparatorResult] = snt
        .to(LazyList)
        .map({ case FileNodeTuple(sf, sn) =>
          // matching firstfilenode with secondfilenode
          val cr = apply(fn, sn)(Context(ff, sf))
          cr match {
            case NodeFound => Subset(ff, sf)
            case nnf: NodeNotFound => FileNotFound(ff, sf, nnf)
          }
        })

      val cr: ComparatorResult = llm
        // terminate the search for this firstfile if a matching secondFile is found
        .find({
          case Subset(_, _) => true
          case _ => false
        })
        // if this firstFile did not match any secondFile, then choose the NodeNotFound that has matched the deepest
        .getOrElse(
          llm.reduce((cr1: ComparatorResult, cr2: ComparatorResult) => {
            (cr1, cr2) match {
              case NodeNotFoundCompare(n1, n2) if n1.length >= n2.length => cr1
              case NodeNotFoundCompare(n1, n2) if n1.length < n2.length => cr2
            }
          })
        )
      cr
    })
    result.foreach(println)
    result
  }

  // checking if two files match
  def apply(fn: Node, sn: Node)(implicit ctx: Context): ComparatorResult = {
    val ccs = ComparingCriteria(fn)
    logger.debug(s"comparing \n$fn\n$sn")
    lazyAllMatch(ccs, (cc: ComparingCriteria) => cc(fn, sn))
  }
}

object NodeNotFoundCompare {
  def unapply(
      cr: (ComparatorResult, ComparatorResult)
  ): Option[(String, String)] = {
    cr match {
      case (
            FileNotFound(_, _, NodeNotFound(n1, _)),
            FileNotFound(_, _, NodeNotFound(n2, _))
          ) =>
        Option((n1, n2))
    }
  }
}
 */
