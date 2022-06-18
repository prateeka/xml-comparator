package com.prateek.xmlcompare.verify

case object XPathFactory {
  type XPath = String

  // Creates xpath for nodes in the form "n1\n2"
  def apply(ns: Seq[scala.xml.Node]): XPath = ns.map(_.label).mkString("\\")

  // Creates xpath for node attributes in the form "n1\n2\@k1" where the xml looks like <n1><n2 k1="x"></n2></n1>
  def appendAttributeKey(node: XPath, attributeKey: String): XPath = s"$node\\@$attributeKey"
}
