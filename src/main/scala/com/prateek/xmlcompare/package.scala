package com.prateek

package object xmlcompare {

  import scala.xml.{Elem, Node}

  //  TODO: this is not working. make it work.
  extension (n: Node) {
    def trim: Node = {
      import scala.xml.Utility
      Utility.trim(n)
    }
  }
}
