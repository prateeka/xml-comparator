package com.prateek

package object xmlcompare {

  import java.io.File

  given stringToFile: Conversion[String, File] = new File(_)
}
