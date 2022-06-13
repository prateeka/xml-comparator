package com.prateek

import scala.xml.{Node, Utility}

import java.io.File

import com.prateek.xmlcompare.read.Valid

package object xmlcompare {

  given stringToFile: Conversion[String, File] = new File(_)

  extension (vs: Seq[Valid]) {
    def mapTrim: Seq[Valid] = vs.map({ case v @ Valid(n, _, _) =>
      v.copy(node = Utility.trim(n))
    })
  }
}
