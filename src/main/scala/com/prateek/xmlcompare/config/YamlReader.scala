package com.prateek.xmlcompare.config

import java.io.InputStreamReader

import cats.syntax.either.*

import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.yaml.parser.parse
import io.circe.Decoder.Result

trait VerificationConfigReader:
  def read(): VerificationConfig

//case class YamlReader() extends App:
object YamlReader extends App:
  val inputStream = getClass.getClassLoader.getResourceAsStream("yaml/criteria-config.yaml")
  val json: Json = parse(new InputStreamReader(inputStream)).get
  println(s"json: $json")
  val vc: VerificationConfig = json.as[VerificationConfig].get
  println(s"vc is $vc")

  given verificationConfigDecoder: Decoder[VerificationConfig] = (c: HCursor) =>
    // each JsonObject corresponds to MessageVerificationConfig
    val v1: Seq[JsonObject] =
      val v11 = c.downField("discoverResponse")
      v11.as[List[JsonObject]].get
    // Seq of Tuple2[XPathRegex, Seq[VerifierId]]
    val v2: Seq[(String, Set[VerifierId])] =
      val v22: Seq[(String, Json)] = v1.flatMap(_.toList)
      v22.map({ case (xpr, json) =>
        val vids = json.as[Set[VerifierId]].get
        (xpr, vids)
      })
    Right(VerificationConfig(v2))

  given verifierIdsDecoder: Decoder[Set[VerifierId]] = (c: HCursor) =>
    val stringVerifierIds = c.as[Set[String]].get
    val vids = stringVerifierIds.map(VerifierId.valueOf)
    Right(vids)

  //  override def read(): VerificationConfig = ???
  extension [A](e: Either[Error, A]) {
    def get: A = e match
      case Left(error)  => throw error
      case Right(value) => value
  }
end YamlReader
