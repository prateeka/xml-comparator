package com.prateek.xmlcompare.verify

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.{Success, Try}
import scala.xml.{Node, NodeSeq, Text}
import scala.xml.Utility.trim

import java.io.File

import com.prateek.xmlcompare.read.{InputFile, Invalid, Valid}
import com.typesafe.scalalogging
import com.typesafe.scalalogging.Logger

// Stores the parent xml node tags
case class VerificationContext(ens: List[String] = Nil) {
  lazy val path: String = ens.mkString(".")

  // TODO: why need a list and can this not be replaced by a string?
  def append(n: Node): VerificationContext = this.copy(ens.:+(n.string))
}

trait Verifier {
  def apply(exp: Node, act: Node)(using ctx: VerificationContext): VerificationResult
}

object Verifier {
  private val logger = com.typesafe.scalalogging.Logger(getClass)

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
        val vrs = new ListBuffer[ActualFileVerificationResult]()
        object InputFileCompare {
          def unapply(av: Valid): Option[ActualFileVerificationResult] = {
            av match
              case Valid(an, af, _) =>
                val vr: VerificationResult = rootVerifier.apply(en, an)(using VerificationContext())
                logger.debug(s"comparing file:$ef with $af yields $vr")
                val afvr = ActualFileVerificationResult(af, vr)
                vrs.append(afvr)
                Option(afvr)
          }
        }

        def maxFileVericationResult: FileVerificationResult = {
          vrs
            .maxByOption({ case ActualFileVerificationResult(_, vr) => vr })
            .map({ case ActualFileVerificationResult(af, vr) =>
              FileVerificationResult(ef, af, vr)
            })
            .get
        }

        val fvr = actValidFiles
          .collectFirst({ case InputFileCompare(ActualFileVerificationResult(af, Match)) =>
            FileVerificationResult(ef, af, Match)
          })
          .getOrElse(maxFileVericationResult)
        fvr
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

  override def apply(exp: Node, act: Node)(using ctx: VerificationContext): VerificationResult = {
    logger.debug(s"comparing expected:${exp.string}")
    val nctx = ctx.append(exp)
    /* Compare `expected` vs `actual` node until first Mismatch occurs. If no Mismatch instances are found then return a
     Match instance
     */
    val vs: Seq[Verifier] = vp(nctx.path)
    object Verification {
      def unapply(v: Verifier): Option[VerificationResult] = Option(v(exp, act)(using nctx))
    }
    val value = {
      vs.collectFirst { case Verification(mm: Mismatch) => mm }
        .getOrElse(Match)
    }
    value
  }
}

case class ChildVerifier(rootVerifier: Verifier = NodeVerifier(VerificationProvider.default))
    extends Verifier {

  private val logger = scalalogging.Logger(getClass)

  override def apply(exp: Node, act: Node)(using ctx: VerificationContext): VerificationResult = {
    /*
     using an iterator helps us continue comparison of expected vs actual node from the point where the previous
     actual node matched. This is in accordance with the assumption that a valid node match includes:
     1. count(actual nodes) >= count(expected nodes).
     2. even though count(actual nodes) >= count(expected nodes) but they must match in order with the expected nodes.
     */
    val iact = act.child.iterator

    object ExpectedChildNodeVerification {

      /*
       Compare an expected child node vs untraversed actual child nodes until first match occurs or all actual child nodes
       are are exhausted. If no Match ever occurs then return the Mismatch instance with the longest context length
       which indicates deepest successful node comparison.
       */
      def unapply(en: Node): Option[VerificationResult] = {
        val vrs = new ListBuffer[VerificationResult]

        // Compare an expected child node with an actual child node
        object NodeCompare {
          def unapply(an: Node): Option[VerificationResult] = {
            val vr = rootVerifier(en, an)
            log(en, an, vr)
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

      private given logger: Logger = scalalogging.Logger(getClass)
    }

    /*
     Compare expected child nodes vs actual child nodes until an expected child node encounters Mismatch error. Return the Mismatch encountered else
     if no Mismatch instances are found then return Match instance implying all the expected child nodes found a matching actual child node.
     */
    val value = {
      //      val ecs: Seq[Node] = trim(exp).child
      val ecs: Seq[Node] = exp.child
      if ecs.nonEmpty then logger.debug(s"${exp.string} children: $ecs")
      val csvr = ecs
        .collectFirst { case ExpectedChildNodeVerification(vr: Mismatch) => vr }
        .getOrElse(Match)
      csvr
    }
    value
  }
}

case object LabelVerifier extends Verifier {

  override def apply(exp: Node, act: Node)(using
      ctx: VerificationContext
  ): VerificationResult =
    val result = (exp, act) match {
      case (e: Text, a: Text) if e.text.equals(a.text) =>
        val vr = Match
        log(e, a, vr)
        vr
      case (e: Text, a: Text) =>
        val vr = NodeTextNotFound(ctx.path)
        log(e, a, vr)
        vr
      case (e: Node, a: Node) if e.label.equals(a.label) =>
        val vr = Match
        log(e, a, vr)
        vr
      case (e: Node, a: Node) =>
        val vr = NodeTextNotFound(ctx.path)
        log(e, a, vr)
        vr
    }
    result

  private given logger: Logger = scalalogging.Logger(getClass)
}

private def log(es: Node, as: Node, vr: VerificationResult)(using
    logger: Logger
): Unit = {
  logger.debug(
    s"comparing expected:${es.string} & ${as.string} yields $vr"
  )
}

extension (n: Node) {
  def string: String = {
    n match {
      case t: Text => t.text
      case n: Node => n.label
    }
  }
}
