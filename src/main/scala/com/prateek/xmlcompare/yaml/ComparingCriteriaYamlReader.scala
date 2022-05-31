package com.prateek.xmlcompare.yaml

import java.io.InputStreamReader

import cats.syntax.either.*

import io.circe.*
import io.circe.generic.semiauto.*

object ComparingCriteriaYamlReader extends App {
  implicit val discoverConfigDecoder: Decoder[Discover] =
    deriveDecoder[Discover]
  implicit val fileTypeDecoder: Decoder[FileTypeConfig] =
    deriveDecoder[FileTypeConfig]
  implicit val nodeVerifiersDecoder: Decoder[NodeVerifiers] =
    deriveDecoder[NodeVerifiers]

  val config =
    getClass.getClassLoader.getResourceAsStream("yaml/criteria-config.yaml")
  val json = yaml.parser.parse(new InputStreamReader(config))
  //  println(json)
  val foo = json
    .leftMap(err => err: Error)
    .flatMap(_.as[FileTypeConfig])
  println(foo)

  case class NodeVerifiers(nodeRegex: String, verifiers: List[String])

  case class Discover(
      defaultInclude: List[String],
      nodeRegex: List[NodeVerifiers]
  )

  case class FileTypeConfig(req: Discover)

}

/** Reads [[ComparingCriteria]] from a yaml file.
  */
/*
object ComparingCriteriaYamlReader {

  type JList = util.ArrayList[String]
  type JMap[T] = util.Map[String, T]
  type StringSet = Set[String]
  type StringToTMap[T] = Map[String, T]

  def apply(file: File): Seq[Criteria] = {
    val contents = {
      val input = new FileInputStream(file)
      val yaml = new Yaml(new Constructor(classOf[JMap[JMap[JList]]]))
      val config: JMap[JMap[JList]] = yaml.load(input)
      config
    }
    val sm: StringToTMap[StringToTMap[StringSet]] = {
      val outerScalaMap = contents.asScala.toMap
      outerScalaMap.map({ case (str, value) =>
        val smjl = value.asScala.toMap
        val smsl = smjl.map({ case (str, list) => str -> list.asScala.toSet })
        str -> smsl
      })
    }
    val criterias = sm.map {
      case ("request", v)  => Criteria(v)
      case ("response", v) => Criteria(v)
    }
    criterias.foreach(println)
    criterias.toSeq
  }
}

case class Criteria(
  exclude: StringSet,
  defaultInclude: StringSet,
  nodeConfig: Map[String, StringSet])

object Criteria {
  def apply(m: StringToTMap[StringSet]): Criteria = {
    val exclude: StringSet = m.getOrElse("exclude", Set.empty)
    val defaultInclude: StringSet = m.getOrElse("defaultInclude", Set.empty)
    val nodeConfig: StringToTMap[StringSet] = m.flatMap({
      case ("exclude", _)        => Map.empty
      case ("defaultInclude", _) => Map.empty
      case (k, v)                => Map(k -> Option(v).getOrElse(Set.empty))
    })
    Criteria(exclude, defaultInclude, nodeConfig)
  }

}

 */
