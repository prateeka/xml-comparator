package com.prateek.xmlcompare.verify

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.{Success, Try}
import scala.xml.{Node, NodeSeq, Text}
import scala.xml.Utility.trim

import java.io.File

import com.prateek.xmlcompare.config.VerifierId
import com.prateek.xmlcompare.read.{InputFile, Invalid, Message, Valid}
import com.prateek.xmlcompare.verify.VerificationContext.NodeQueue
import com.prateek.xmlcompare.verify.Verifier.getClass
import com.prateek.xmlcompare.verify.XPathFactory.{appendAttributeKey, appendText, XPath}
import com.typesafe.scalalogging
import com.typesafe.scalalogging.Logger

// Stores the parent xml node tags
case class VerificationContext(msg: Message, nq: NodeQueue = Nil):

  lazy val xpath: XPath = XPathFactory(nq)

  // TODO: why need a list and can this not be replaced by a string?
  //  def append(n: Node): VerificationContext = this.copy(ens.:+(n.string))
  def append(n: Node): VerificationContext = this.copy(nq = nq.:+(n))
end VerificationContext

object VerificationContext:
  private type NodeQueue = List[Node]
end VerificationContext

trait Verifier:
  val id: VerifierId

  def apply(exp: Node, act: Node)(using ctx: VerificationContext): VerificationResult
end Verifier

object Verifier:
  val verifiers: Seq[Verifier] = Seq(LabelVerifier(), TextVerifier(), AttributeVerifier(), cv)
  private val nv: NodeVerifier = NodeVerifier(verifiers)
  private val cv: ChildVerifier = ChildVerifier(nv)
  private val logger = scalalogging.Logger(getClass)

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
      rootVerifier: Verifier = NodeVerifier(verifiers)
  ): Seq[FileVerificationResult] =
    type ActualFileVerificationResult = (File, VerificationResult)

    val fvrs: Seq[FileVerificationResult] = expValidFiles
      .map({ case Valid(en, ef, emsg) =>
        val vrs = new ListBuffer[ActualFileVerificationResult]()
        object InputFileCompare {
          def unapply(av: Valid): Option[ActualFileVerificationResult] = {
            av match
              case Valid(an, af, _) =>
                val vr: VerificationResult =
                  rootVerifier.apply(en, an)(using VerificationContext(msg = emsg))
                logger.debug(s"comparing file:$ef with $af yields $vr")
                val afvr = new ActualFileVerificationResult(af, vr)
                vrs.append(afvr)
                Option(afvr)
          }
        }

        def maxFileVericationResult: FileVerificationResult = vrs
          .maxByOption({ case ActualFileVerificationResult(_, vr) => vr })
          .map({ case ActualFileVerificationResult(af, vr) =>
            FileVerificationResult(ef, af, vr)
          })
          .get

        val fvr = actValidFiles
          .collectFirst({ case InputFileCompare(ActualFileVerificationResult(af, Match)) =>
            FileVerificationResult(ef, af, Match)
          })
          .getOrElse(maxFileVericationResult)
        fvr
      })
    fvrs
  end apply
end Verifier

/** Compares two nodes using a list of [[Verifier]] approved by a [[VerificationPredicate]]. It follow a fail fast
  * strategy where it stops comparison after the first [[Mismatch]] encountered.
  *
  * @param vs list of all [[Verifier]] barring [[VerifierId.Ignore]]
  */
class NodeVerifier(vs: => Seq[Verifier]) extends Verifier {
  override val id: VerifierId = VerifierId.Node
  private val logger = scalalogging.Logger(getClass)

  override def apply(exp: Node, act: Node)(using ctx: VerificationContext): VerificationResult = {
    logger.debug(s"comparing expected:${exp.string}")
    object Verification {
      def unapply(v: Verifier): Option[VerificationResult] = Option(v(exp, act)(using ctx))
    }
    val value = {
      vs.collectFirst { case Verification(mm: Mismatch) => mm }
        .getOrElse(Match)
    }
    value
  }
}

/** Retrieves and iterates through the child nodes of the expected and actual nodes
  *
  * @param rootVerifier usually would be [[NodeVerifier]]
  * @param vp           confirms if a [[Verifier]] can be used for every (nested) [[Node]]
  */
class ChildVerifier(
    rootVerifier: => Verifier,
    vp: VerificationPredicate = VerificationPredicate.instance
) extends Verifier {
  override val id: VerifierId = VerifierId.Child
  private val logger = scalalogging.Logger(getClass)

  override def apply(exp: Node, act: Node)(using ctx: VerificationContext): VerificationResult = {
    val nctx = ctx.append(exp)
    // checking if child verification for expected node can continue
    if vp(nctx.msg, id, nctx.xpath) then
      logger.debug(s"${nctx.msg},$id,${nctx.xpath}  is approved")
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
              val vr = rootVerifier(en, an)(using nctx)
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
              val maybeFound = Option(NodeNotFound(s"${nctx.append(en).xpath}"))
              val result = vrs.maxOption.orElse(maybeFound)
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
        val ecs: Seq[Node] = exp.child
        if ecs.nonEmpty then logger.debug(s"${exp.string} children: $ecs")
        val csvr = ecs
          .collectFirst { case ExpectedChildNodeVerification(vr: Mismatch) => vr }
          .getOrElse(Match)
        csvr
      }
      value
    else
      logger.debug(s"${nctx.msg},$id,${nctx.xpath}  is not approved")
      Match
    end if
  }
}

// Verifies Elem.label or Text.text depending on the type of Node passed
case class LabelVerifier(vp: VerificationPredicate = VerificationPredicate.instance)
    extends Verifier {
  override val id: VerifierId = VerifierId.Label

  override def apply(exp: Node, act: Node)(using ctx: VerificationContext): VerificationResult =
    def compare(et: String, at: String, xp: XPath) =
      if et.equals(at) then Match else NodeNotFound(s"$xp")
    end compare

    val result = (exp, act) match
      case (Text(_), Text(_)) => Match
      case (e: Node, a: Node) =>
        val xp = ctx.append(e).xpath
        val vr = if vp(ctx.msg, id, xp) then compare(e.label, a.label, xp) else Match
        vr
    result

  private given logger: Logger = scalalogging.Logger(getClass)
}

case class TextVerifier(vp: VerificationPredicate = VerificationPredicate.instance)
    extends Verifier {
  override val id: VerifierId = VerifierId.Text

  override def apply(exp: Node, act: Node)(using ctx: VerificationContext): VerificationResult =
    object TextComparator:
      def unapply(t2: (Node, Node)): Option[VerificationResult] = t2 match
        case (Text(et), Text(at)) =>
          val xp = ctx.xpath.appendText()
          val vr = if vp(ctx.msg, id, xp) then compare(et, at, xp) else Match
          Option(vr)
        case _ => None
      end unapply

      private def compare(et: String, at: String, xp: XPath) =
        if et.equals(at) then Match else NodeTextNotFound(s"$xp")
      end compare
    end TextComparator

    val result = (exp, act) match
      case TextComparator(vr) =>
        log(exp, act, vr)
        vr
      case (_: Node, _: Node) => Match
    result

  private given logger: Logger = scalalogging.Logger(getClass)
}

case class AttributeVerifier(vp: VerificationPredicate = VerificationPredicate.instance)
    extends Verifier:
  override val id: VerifierId = VerifierId.Attribute

  override def apply(exp: Node, act: Node)(using ctx: VerificationContext): VerificationResult =
    val (expAttr: Set[(String, String)], actAttr: Set[(String, String)]) =
      (exp.attributes.asAttrMap.toSet, act.attributes.asAttrMap.toSet)
    //      TODO: currently reports the first attributeKey missing. update the Verifier.apply to return Seq[VerificationResult]
    val vr = expAttr
      .filter { case (ek, _) =>
        val xp = ctx.xpath.appendAttributeKey(ek)
        vp(ctx.msg, id, xp)
      }
      .find({ case (ek, ev) => !actAttr.contains(ek, ev) })
      .map({ case (ek, _) =>
        AttributeMissing(s"${ctx.append(exp).xpath.appendAttributeKey(ek)}")
      })
      .getOrElse(Match)
    logger.debug(s"comparing attributes: $expAttr & $actAttr yields $vr")
    vr
  end apply
  private given logger: Logger = scalalogging.Logger(getClass)
end AttributeVerifier

private def log(es: Node, as: Node, vr: VerificationResult)(using logger: Logger): Unit = {
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
