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
  private val nrc: NodeVerifiersConfig = json.as[NodeVerifiersConfig].get
  println(s"nrc: ${nrc.nodeToVerifiersMap}")

  case class NodeVerifiersConfig(nodeVerifiers: List[JsonObject]) {
    val nodeToVerifiersMap: Map[String, List[String]] = {
      val nodeJsonTuples: Seq[(String, Json)] = nodeVerifiers
        .flatMap(f => f.toMap)
      val nodeToJsonMap: Map[String, Json] = nodeJsonTuples.toMap
      nodeToJsonMap
        .map({ case (k, v) =>
          val x = v.as[List[String]].get
          (k, x)
        })
    }
  }

  object NodeVerifiersConfig {
    implicit val nodeVerifiersConfigDecoder: Decoder[NodeVerifiersConfig] =
      deriveDecoder[NodeVerifiersConfig]
  }

  extension [A](e: Either[Error, A]) {
    def get: A = {
      e match
        case Left(error)  => throw error
        case Right(value) => value
    }
  }
}
