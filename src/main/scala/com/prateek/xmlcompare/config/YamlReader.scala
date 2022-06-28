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
  val dr: DiscoverResponse = json.as[DiscoverResponse].get

  given discoverResponseDecoder: Decoder[DiscoverResponse] = (c: HCursor) =>
    Right(DiscoverResponse(NodeConfig(c, "discoverResponse")))

  given MVCDecoder: Decoder[MVC] = (c: HCursor) =>
    c.as[(String, Json)].get
    Right(DiscoverResponse(NodeConfig(c, "discoverResponse")))
    ???

  given verifierIdDecoder: Decoder[Set[VerifierId]] = (c: HCursor) =>
    val stringVerifierIds = c.as[Set[String]].get
    val vids = stringVerifierIds.map(VerifierId.valueOf)
    Right(vids)

  println(s"discoverResponseDecoder: $dr")
  //  override def read(): VerificationConfig = ???

  case class NodeConfig(n: String, v: List[String])

  case class DiscoverResponse(discoverResponse: List[NodeConfig])

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
        val value: Seq[JsonObject] = nodeVerifiersJson.get
        val value2 = value
          .flatMap((jo: JsonObject) => jo.toList)
          .map({ case (xpr, json) =>
            val vids = json.as[Set[VerifierId]].get
            (xpr, vids)
          })
        val vc = VerificationConfig(value2)
        println(vc)
        val value1: Seq[(String, Json)] = value.flatMap((f: JsonObject) => f.toMap)
        value1
      }
      val nvs: List[NodeConfig] = nodeJsonTuples
        .map({ case (k, v) =>
          //          v.as[Set[VerifierId]].get.foreach(println)
          val verifiers = v.as[List[String]].get
          val config = NodeConfig(k, verifiers)
          config
        })
        .toList
      nvs
    end apply
  end NodeConfig

end YamlReader
