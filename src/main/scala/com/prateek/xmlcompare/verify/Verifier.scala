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
//TODO: why need a list and can this not be replaced by a string?
  def append(n: Node): Context = {
    this.copy(en.:+(n.label))
  }
}

trait Verifier {
  def apply(exp: Node, act: Node)(using
      ctx: Context
  ): VerificationResult
}

object Verifier {

  /** Compares each actual file with all the expected files to identify the
    * following:
    * 1. Lists all the expected files an actual file matches.
    * 2. If the actual file is not matching any expected file, then indicate the same
    * while listing the node where the mismatch occurred.
    *
    * @param ernss        list of expected files
    * @param arnss        list of actual files
    * @param rootVerifier provides [[Verifier]] for the absolute node tag
    * @return VerificationResult describing if the two files match else for the best mismatch, the node where the mismatch occurred.
    */
  def apply(
      ernss: Seq[RootNodeSource],
      arnss: Seq[RootNodeSource],
      rootVerifier: Verifier = NodeVerifier(VerificationProvider.default)
  ): Seq[FileVerificationResult] = {
    case class FileVerificationResultTuple2(f: File, vr: VerificationResult)

    val value = ernss
      .map({ case RootNodeSource(ef, Success(en)) =>
        //        TODO: stop at the first exp & act match instead of going through all the act files
        // returns a result of an expFile match with all the actFiles
        val tuple2: Seq[FileVerificationResultTuple2] = arnss.map({
          case RootNodeSource(af, Success(an)) =>
            val vr: VerificationResult =
              rootVerifier.apply(en, an)(using Context())
            FileVerificationResultTuple2(af, vr)
        })
        // determine the best actFile match for an expFile
        val fvr = tuple2.maxBy(_.vr) match
          case FileVerificationResultTuple2(af, vr) =>
            FileVerificationResult(ef, af, vr)
        fvr
      })
    value
  }
}

/** Compares two nodes using a list of [[Verifiction]] provided by a [[VerificationProvider]]. It follow a fail fast
  * strategy where it stops comparison after the first [[Mismatch]] encountered.
  *
  * @param vp [[VerificationProvider]] provides a list of [[Verifier]] for every (nested) [[Node]]
  */
case class NodeVerifier(vp: VerificationProvider) extends Verifier {

  override def apply(exp: Node, act: Node)(using
      ctx: Context
  ): VerificationResult = {
    val nctx = ctx.append(exp)
    /*
     Compare `expected` vs `actual` node until first Mismatch occurs. If no Mismatch instances are found then return a
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
  override def apply(exp: Node, act: Node)(using
      ctx: Context
  ): VerificationResult = {
    /*
     using an iterator helps us continue comparison of expected vs actual node from the point where the previous
     actual node matched. This is in accordance with the assumption that a valid node match includes:
     1. count(actual nodes) >= count(expected nodes).
     2. even though count(actual nodes) >= count(expected nodes) but they must match in order with the expected nodes.
     */
    val iact = {
      val nact = trim(act)
      nact.child.iterator
    }

    object ExpectedChildNodeVerification {
      /*
       Compare an expected child node vs untraversed actual child nodes until first match occurs or all actual child nodes
       are are exhausted. If no Match ever occurs then return the Mismatch instance with the longest context length
       which indicates deepest successful node comparison.
       */
      def unapply(en: Node): Option[VerificationResult] = {
        val lb = new ListBuffer[VerificationResult]

        // Compare an expected child node with an actual child node
        object NodeCompare {
          def unapply(an: Node): Option[VerificationResult] = {
            val vr = rootVerifier(en, an)
            lb.append(vr)
            Option(vr)
          }
        }

        /*
         Returns the deepest Mismatch VerificationResult. If an expected child node cannot be compared with any actual node as all the
         actual nodes have been traversed then create a new NodeNotFound(ctx.append(en).path) to be returned.
         */
        def maxDepthVerificationResult: VerificationResult = {
          val result = lb.maxOption.getOrElse(NodeNotFound(ctx.append(en).path))
          result
        }

        val value: Option[VerificationResult] = Option(
          iact
            .collectFirst({ case NodeCompare(m: Match.type) => m })
            .getOrElse(maxDepthVerificationResult)
        )
        value
      }
    }

    /*
     Compare expected child nodes vs actual child nodes until an expected child node encounters Mismatch error. Return the Mismatch encountered else
     if no Mismatch instances are found then return Match instance implying all the expected child nodes found a matching actual child node.
     */
    val value = {
      val cs = trim(exp).child
      val csvr = cs
        .collectFirst { case ExpectedChildNodeVerification(vr: Mismatch) => vr }
        .getOrElse(Match)
      csvr
    }
    value
  }
}

case object LabelVerifier extends Verifier {
  override def apply(exp: Node, act: Node)(using
      ctx: Context
  ): VerificationResult =
    //    TODO: see if this can be made into a predicate method which is present in Verification and the predicate function is provided by the specializations
    if exp.label.equals(act.label) then Match
    else {
      NodeNotFound(ctx.path)
    }
}

trait VerificationResult

object VerificationResult {
  given vo: Ordering[VerificationResult] with
    override def compare(
        xvr: VerificationResult,
        yvr: VerificationResult
    ): Int = (xvr, yvr) match
      case (_ @Match, _: VerificationResult) => 1
      case (_: VerificationResult, _ @Match) => -1
      case (a: Mismatch, b: Mismatch) =>
        a.label().length.compare(b.label().length)
}

case object Match extends VerificationResult

trait Mismatch(n: String) extends VerificationResult {
  def label(): String = n
}

case class NodeNotFound(n: String) extends Mismatch(n)

case class NodeTextNotFound(n: String) extends Mismatch(n)

case class FileVerificationResult(
    exp: File,
    act: File,
    verificationResult: VerificationResult
)
