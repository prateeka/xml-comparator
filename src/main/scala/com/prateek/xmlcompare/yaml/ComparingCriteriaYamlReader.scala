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
  private val nrc: NodeRegexConfig = json.as[NodeRegexConfig].get
  println(s"nrc: ${nrc.nodeRegexMap}")

  case class NodeRegexConfig(nodeRegex: List[JsonObject]) {
    val nodeRegexMap: Map[String, List[String]] = {
      val nodeToRegex: Map[String, Json] = nodeRegex
        .flatMap(f => f.toMap)
        .toMap
      val regexIterator: Map[String, List[String]] =
        nodeToRegex.map({ case (k, v) =>
          val x = v.as[List[String]].get
          (k, x)
        })
      regexIterator
    }
  }

  object NodeRegexConfig {
    implicit val nodeRegexConfigDecoder: Decoder[NodeRegexConfig] =
      deriveDecoder[NodeRegexConfig]
  }

  extension [A](e: Either[Error, A]) {
    def get: A = {
      e match
        case Left(error)  => throw error
        case Right(value) => value
    }
  }
}
