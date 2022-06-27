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
  val dr: DiscoverResponseConfig = json.as[DiscoverResponseConfig].get

  given discoverResponseDecoder: Decoder[DiscoverResponseConfig] = (c: HCursor) =>
    Right(DiscoverResponseConfig(NodeConfig(c, "discoverResponse")))

  println(s"discoverResponseDecoder: $dr")
  //  override def read(): VerificationConfig = ???

  case class NodeConfig(n: String, v: List[String])

  case class DiscoverResponseConfig(discoverResponse: List[NodeConfig])

  extension [A](e: Either[Error, A]) {
    def get: A = e match
      case Left(error)  => throw error
      case Right(value) => value
  }

  object NodeConfig:
    def apply(c: HCursor, label: String): List[NodeConfig] =
      val nodeVerifiersJson: Result[List[JsonObject]] = {
        val cursor = c.downField(label)
        val value = cursor.as[List[JsonObject]]
        value
      }
      val nodeJsonTuples: Seq[(String, Json)] = {
        val value = nodeVerifiersJson.get
        val value1 = value.flatMap(f => f.toMap)
        value1
      }
      val nvs: List[NodeConfig] = nodeJsonTuples
        .map({ case (k, v) =>
          val verifiers = v.as[List[String]].get
          val config = NodeConfig(k, verifiers)
          config
        })
        .toList
      nvs
    end apply
  end NodeConfig

end YamlReader
