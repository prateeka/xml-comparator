package com.prateek.xmlcompare.verify

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.{Success, Try}
import scala.xml.{Node, NodeSeq}
import scala.xml.Utility.trim

import java.io.File

import com.prateek.xmlcompare.read.Valid

// Stores the parent xml node tags
case class VerificationContext(en: List[String] = Nil) {
  lazy val path: String = en.mkString(".")

  // TODO: why need a list and can this not be replaced by a string?
  def append(n: Node): VerificationContext = {
    this.copy(en.:+(n.label))
  }
}

trait Verifier {
  def apply(exp: Node, act: Node)(using
      ctx: VerificationContext
  ): VerificationResult
}

object Verifier {

  /** Compares each expected file with all the actual files to identify the following:
    * 1. Lists all the expected files an actual file matches.
    * 2. If the actual file is not matching any expected file, then indicate the same
    * while listing the node where the mismatch occurred.
    *
    * @param expValidFiles list of expected files
    * @param actValidFiles list of actual files
    * @param rootVerifier  provides [[Verifier]] for the absolute node tag
    * @return VerificationResult describing if the two files match else for the best mismatch, the node where the mismatch occurred.
    */
  def apply(
      expValidFiles: Seq[Valid],
      actValidFiles: Seq[Valid],
      rootVerifier: Verifier = NodeVerifier(VerificationProvider.default)
  ): Seq[FileVerificationResult] = {
    case class ActualFileVerificationResult(f: File, vr: VerificationResult)

    val fvrs: Seq[FileVerificationResult] = expValidFiles
      .map({ case Valid(en, ef, _) =>
        //        TODO: stop at the first exp & act match instead of going through all the act files
        // returns a result of an expFile match with all the actFiles
        val afvrs: Seq[ActualFileVerificationResult] = actValidFiles.map({
          case Valid(an, af, _) =>
            val vr: VerificationResult =
              rootVerifier.apply(en, an)(using VerificationContext())
            ActualFileVerificationResult(af, vr)
        })
        // determine the best actFile match for an expFile
        val bestResult = afvrs.maxBy(_.vr) match
          case ActualFileVerificationResult(af, vr) =>
            FileVerificationResult(ef, af, vr)
        bestResult
      })
    fvrs
  }
}

/** Compares two nodes using a list of [[Verifiction]] provided by a [[VerificationProvider]]. It follow a fail fast
  * strategy where it stops comparison after the first [[Mismatch]] encountered.
  *
  * @param vp [[VerificationProvider]] provides a list of [[Verifier]] for every (nested) [[Node]]
  */
case class NodeVerifier(vp: VerificationProvider) extends Verifier {
  private val logger = com.typesafe.scalalogging.Logger(getClass)

  override def apply(exp: Node, act: Node)(using
      ctx: VerificationContext
  ): VerificationResult = {
    logger.info(s"comparing expected:${exp.label}")
    val nctx = ctx.append(exp)
    /* Compare `expected` vs `actual` node until first Mismatch occurs. If no Mismatch instances are found then return a
     Match instance
     */
    val vs: Seq[Verifier] = vp(nctx.path)
    object Verification {
      def unapply(v: Verifier): Option[VerificationResult] = Option(
        v(exp, act)(using nctx)
      )
    }
    val value = {
      vs.collectFirst { case Verification(mm: Mismatch) => mm }
        .getOrElse(Match)
    }
    value
  }
}

case class ChildVerifier(
    rootVerifier: Verifier = NodeVerifier(VerificationProvider.default)
) extends Verifier {
  private val logger = com.typesafe.scalalogging.Logger(getClass)

  override def apply(exp: Node, act: Node)(using
      ctx: VerificationContext
  ): VerificationResult = {
    /*
     using an iterator helps us continue comparison of expected vs actual node from the point where the previous
     actual node matched. This is in accordance with the assumption that a valid node match includes:
     1. count(actual nodes) >= count(expected nodes).
     2. even though count(actual nodes) >= count(expected nodes) but they must match in order with the expected nodes.
     */
    val iact = {
      act.child.iterator
      /*
      val nact = trim(act)
      nact.child.iterator
       */
    }

    object ExpectedChildNodeVerification {
      /*
       Compare an expected child node vs untraversed actual child nodes until first match occurs or all actual child nodes
       are are exhausted. If no Match ever occurs then return the Mismatch instance with the longest context length
       which indicates deepest successful node comparison.
       */
      def unapply(en: Node): Option[VerificationResult] = {
        val vrs = new ListBuffer[VerificationResult]
        logger.info(s"comparing expected:${en.label}")

        // Compare an expected child node with an actual child node
        object NodeCompare {
          def unapply(an: Node): Option[VerificationResult] = {
            val vr = rootVerifier(en, an)
            vrs.append(vr)
            Option(vr)
          }
        }

        val value: Option[VerificationResult] = {
          /*
           Returns the deepest Mismatch VerificationResult. If an expected child node cannot be compared with any actual node as all the
           actual nodes have been traversed then create a new NodeNotFound(ctx.append(en).path) to be returned.
           */
          def maxDepthVerificationResult: Option[VerificationResult] = {
            val result =
              vrs.maxOption.orElse(Option(NodeNotFound(ctx.append(en).path)))
            result
          }

          iact
            .collectFirst({ case NodeCompare(m: Match.type) => m })
            .orElse(maxDepthVerificationResult)
        }
        value
      }
    }

    /*
     Compare expected child nodes vs actual child nodes until an expected child node encounters Mismatch error. Return the Mismatch encountered else
     if no Mismatch instances are found then return Match instance implying all the expected child nodes found a matching actual child node.
     */
    val value = {
      //      val ecs: Seq[Node] = trim(exp).child
      val ecs: Seq[Node] = exp.child
      logger.info(s"${exp.label} children are $ecs")
      val csvr = ecs
        .collectFirst { case ExpectedChildNodeVerification(vr: Mismatch) => vr }
        .getOrElse(Match)
      csvr
    }
    value
  }
}

case object LabelVerifier extends Verifier {
  private val logger = com.typesafe.scalalogging.Logger(getClass)

  override def apply(exp: Node, act: Node)(using
      ctx: VerificationContext
  ): VerificationResult =
    //    TODO: see if this can be made into a predicate method which is present in Verification and the predicate function is provided by the specializations
    val result =
      if exp.label.equals(act.label) then Match
      else {
        NodeTextNotFound(ctx.path)
      }
    logger.info(
      s"comparing expected:${exp.label} & ${act.label} yields $result"
    )
    result
}
