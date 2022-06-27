package com.prateek.xmlcompare.config

import java.io.InputStreamReader

import cats.syntax.either.*

import io.circe.*
import io.circe.generic.semiauto.*

trait VerificationConfigReader:
  def read(): VerificationConfig

object ComparingCriteriaYamlReader extends VerificationConfigReader:
  val config = getClass.getClassLoader.getResourceAsStream("yaml/criteria-config.yaml")
  val json: Json = yaml.parser.parse(new InputStreamReader(config)).get
  println(s"json $json")

  implicit val discoverResponseDecoder: Decoder[DiscoverResponseConfig] = (c: HCursor) =>
    Right(DiscoverResponseConfig(NodeConfig(c, "discoverResponse")))

  val dr: DiscoverResponseConfig = json.as[DiscoverResponseConfig].get
  println(s"$dr")

  override def read(): VerificationConfig = ???

  case class NodeConfig(n: String, v: List[String])

  case class DiscoverResponseConfig(discoverResponse: List[NodeConfig])

  extension [A](e: Either[Error, A]) {
    def get: A = e match
      case Left(error)  => throw error
      case Right(value) => value
  }

  object NodeConfig {
    def apply(c: HCursor, label: String): List[NodeConfig] = {
      import io.circe.Decoder.Result
      val nodeVerifiersJson: Result[List[JsonObject]] = c.downField(label).as[List[JsonObject]]
      val nodeJsonTuples: Seq[(String, Json)] = nodeVerifiersJson.get.flatMap(f => f.toMap)
      val nvs: List[NodeConfig] = nodeJsonTuples
        .map({ case (k, v) =>
          val verifiers = v.as[List[String]].get
          NodeConfig(k, verifiers)
        })
        .toList
      nvs
    }
  }
end ComparingCriteriaYamlReader
