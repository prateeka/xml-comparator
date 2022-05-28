package com.prateek.xmlcompare.verify

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.{Success, Try}
import scala.xml.{Node, NodeSeq}
import scala.xml.Utility.trim

import java.io.File

/** Stores the file and its corresponding xml node or a failure reading the file. If the file is valid then it is used for xml comparison.
  * tns: xml node like DiscoverRequest, DiscoverResponse or other valid nodes. The file is valid if it contains one of these nodes else is invalid.
  */
case class RootNodeSource(file: File, tns: Try[Node])

// Stores the parent xml node tags
case class Context(en: List[String] = Nil) {
  lazy val path: String = en.mkString(",")

  def append(n: Node): Context = {
    this.copy(en.:+(n.label))
  }
}

trait Verification {
  def apply(exp: Node, act: Node)(using
      ctx: Context
  ): VerificationResult
}

object Verification {

  /** Compares each actual file with all the expected files to identify the
    * following:
    * 1. Lists all the expected files an actual file matches.
    * 2. If the actual file is not matching any expected file, then indicate the same
    * while listing the node where the mismatch occurred.
    *
    * @param exp list of expected files
    * @param act list of actual files
    * @return VerificationResult describing if the two sources match else the node
    *         where the mismatch occurred.
    */
  def apply(
      exp: Seq[RootNodeSource],
      act: Seq[RootNodeSource],
      vp: VerificationProvider = VerificationProvider.default
  ): Seq[SourceVerificationResult] = {
    exp
      .flatMap({ case RootNodeSource(ef, Success(en)) =>
        val v = vp(en.label).head
        val tuple2: Seq[(File, VerificationResult)] =
          act.map({ case RootNodeSource(af, Success(an)) =>
            val vr = v.apply(en, an)(using Context())
            (af, vr)
          })
        tuple2.map({ case (af, vr) =>
          SourceVerificationResult(ef, af, vr)
        })
      })
  }
}

/** Compares two nodes using a list of [[Verifiction]] provided by a [[VerificationProvider]]. It follow a fail fast
  * strategy where it stops comparison after the first [[Mismatch]] encountered.
  *
  * @param vp [[VerificationProvider]] provides a list of [[Verification]] for every (nested) [[Node]]
  */
case class NodeVerification(vp: VerificationProvider) extends Verification {

  override def apply(exp: Node, act: Node)(using
      ctx: Context
  ): VerificationResult = {
    val nctx = ctx.append(exp)
    /*
     Compare expected vs actual node until first Mismatch occurs. If no Mismatch instances are found then return
     Match instance
     */
    val vs = vp(nctx.path)
    object VerificationFailure {
      def unapply(v: Verification): Option[VerificationResult] = Option(
        v(exp, act)(using nctx)
      )
    }
    val maybeMismatch = vs.collectFirst({
      case VerificationFailure(mm: Mismatch) => mm
    })
    maybeMismatch.getOrElse(Match)
  }
}

case class ChildVerification(v: Verification) extends Verification {
  override def apply(exp: Node, act: Node)(using
      ctx: Context
  ): VerificationResult =
    /* using an iterator helps us continue comparison of expected vs actual node from the point where the previous
      actual node matched. This is in accordance with the assumption that a valid node match includes:
      1. actual nodes more in number than the expected nodes.
      2. even though actual nodes are more in number but they must match in order with the expected nodes.
     */
    val nact = trim(act)
    val nait = nact.child.iterator

    object ChildNodeNotFound {
      /* Compare expected vs actual node until first match occurs or all actual nodes to compare are exhausted. If no
       Match ever occurs then return the Mismatch instance with the longest context length which indicates deepest successful
       node comparison.
       */
      def unapply(en: Node): Option[VerificationResult] = {
        val lb = new ListBuffer[VerificationResult]
        if !nait.hasNext then {
          val nnf = NodeNotFound(ctx.append(en).path)
          lb.append(nnf)
        } else {
          while (nait.hasNext && !lb.lastOption.contains(Match)) {
            val an = nait.next()
            lb.append(v(en, an))
          }
        }
        lb.maxOption.collect({ case mm: Mismatch => mm })
      }
    }

    /*
     Compare expected vs actual node until first Mismatch occurs. If no Mismatch instances are found then return
     Match instance else return Mismatch instance.
     */
    val nexp = trim(exp)
    val cnvr = nexp.child.collectFirst { case ChildNodeNotFound(vr: Mismatch) =>
      vr
    }
    cnvr.getOrElse(Match)
}

case object LabelVerification extends Verification {
  override def apply(exp: Node, act: Node)(using
      ctx: Context
  ): VerificationResult =
    if exp.label.equals(act.label) then Match
    else {
      NodeNotFound(ctx.path)
    }
}

trait VerificationResult

object VerificationResult {
  given vo: Ordering[VerificationResult] = {
    case (_ @Match, _: VerificationResult) => 1
    case (_: VerificationResult, _ @Match) => -1
    case (a: Mismatch, b: Mismatch) =>
      a.label().length.compare(b.label().length)
  }
}

case object Match extends VerificationResult

trait Mismatch(n: String) extends VerificationResult {
  def label(): String = n
}

case class NodeNotFound(n: String) extends Mismatch(n)

case class NodeTextNotFound(n: String) extends Mismatch(n)

case class SourceVerificationResult(
    exp: File,
    act: File,
    verificationResult: VerificationResult
)
