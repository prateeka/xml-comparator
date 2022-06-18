package com.prateek.xmlcompare.verify

case object XPathFormatter {
  // Creates xpath for nodes in the form "n1\n2"
  def apply(ns: Seq[scala.xml.Node]): String = ns.map(_.label).mkString("\\")

  // Creates xpath for node attributes in the form "n1\n2\@k1" where the xml looks like <n1><n2 k1="x"></n2></n1>
  def attribute(node: String, attributeKey: String): String = s"$node\\@$attributeKey"
}
