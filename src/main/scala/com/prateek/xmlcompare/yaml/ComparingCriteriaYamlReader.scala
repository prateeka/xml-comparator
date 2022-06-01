package com.prateek.xmlcompare.yaml

import java.io.InputStreamReader

import cats.syntax.either.*

import io.circe.*
import io.circe.generic.semiauto.*

object ComparingCriteriaYamlReader extends App {

  val config =
    getClass.getClassLoader.getResourceAsStream("yaml/criteria-config.yaml")
  val json: Json = yaml.parser.parse(new InputStreamReader(config)).get
  println(s"json $json")

  implicit val discoverResponseDecoder: Decoder[DiscoverResponse] =
    (c: HCursor) => {
      Right(DiscoverResponse(NodeVerifier(c, "discoverResponse")))
    }

  val dr: DiscoverResponse = json.as[DiscoverResponse].get
  println(s"$dr")

  case class NodeVerifier(n: String, v: List[String])

  case class DiscoverResponse(discoverResponse: List[NodeVerifier])

  object NodeVerifier {
    def apply(c: HCursor, label: String): List[NodeVerifier] = {
      import io.circe.Decoder.Result
      val nodeVerifiersJson: Result[List[JsonObject]] =
        c.downField(label).as[List[JsonObject]]
      val nodeJsonTuples: Seq[(String, Json)] =
        nodeVerifiersJson.get.flatMap(f => f.toMap)
      val nvs: List[NodeVerifier] = nodeJsonTuples
        .map({ case (k, v) =>
          val verifiers = v.as[List[String]].get
          NodeVerifier(k, verifiers)
        })
        .toList
      nvs
    }
  }
  extension [A](e: Either[Error, A]) {
    def get: A = {
      e match
        case Left(error)  => throw error
        case Right(value) => value
    }
  }
}
