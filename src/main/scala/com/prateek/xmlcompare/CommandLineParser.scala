package com.prateek.xmlcompare

import org.rogach.scallop.{
  fileConverter,
  ScallopConf,
  ScallopOption,
  ValueConverter
}

class CommandLineParser[T](arguments: Seq[String])(using ValueConverter[T])
    extends ScallopConf(arguments) {
  val expected: ScallopOption[T] = opt[T](required = true)
  val actual: ScallopOption[T] = opt[T](required = true)
  verify()
}
