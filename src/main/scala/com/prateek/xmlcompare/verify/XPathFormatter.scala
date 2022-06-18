package com.prateek.xmlcompare.verify

case object XPathFormatter {
  def apply(ns: Seq[scala.xml.Node]): String = ns.map(_.label).mkString("\\")

  def attribute(node: String, attributeKey: String): String = s"$node\\@$attributeKey"
}
