package com.prateek.xmlcompare.config

import scala.io.Source

import java.io.{File, FileInputStream, InputStreamReader}
import java.net.URL

import cats.syntax.either.*

import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.yaml.parser.parse
import io.circe.Decoder.Result

trait VerificationConfigReader extends (File => VerificationConfig)

object VerificationConfigReader:
  def apply(config: File): VerificationConfig = YamlReader(config)
end VerificationConfigReader

extension [A](e: Either[Error, A])
  def get: A = e match
    case Left(error)  => throw error
    case Right(value) => value
end extension

given verificationConfigDecoder: Decoder[VerificationConfig] = (c: HCursor) =>
  // each JsonObject corresponds to MessageVerificationConfig
  val v1: Seq[JsonObject] =
    val v11 = c.downField("discoverResponse")
    v11.as[List[JsonObject]].get
  val v2: Seq[(XPathRegex, Set[VerifierId])] =
    val v22: Seq[(XPathRegex, Json)] = v1.flatMap(_.toList)
    v22.map({ case (xpr, json) =>
      val vids = json.as[Set[VerifierId]].get
      (xpr, vids)
    })
  Right(VerificationConfig(v2))
end verificationConfigDecoder

given verifierIdsDecoder: Decoder[Set[VerifierId]] = (c: HCursor) =>
  val stringVerifierIds = c.as[Set[String]].get
  val vids = stringVerifierIds.map(VerifierId.valueOf)
  Right(vids)
end verifierIdsDecoder

case object YamlReader extends VerificationConfigReader:
  private val logger = com.typesafe.scalalogging.Logger(getClass)

  override def apply(config: File): VerificationConfig =
    val inputStream = new FileInputStream(config)
    val json: Json = parse(new InputStreamReader(inputStream)).get
    logger.debug(s"json: $json")
    val vc: VerificationConfig = json.as[VerificationConfig].get
    logger.debug(s"vc is $vc")
    vc
end YamlReader
