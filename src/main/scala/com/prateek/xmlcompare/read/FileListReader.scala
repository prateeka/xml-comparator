package com.prateek.xmlcompare.read

import java.io.File

/** Validates f: [[File]] passed exists and accordingly returns:
  *   1. If "f" is a directory, list of xml files inside "f". 2. Else if "f" is
  *      xml file then a singleton Seq(f). 3. Else, return an empty sequence.
  */
trait FileListReader extends (File => Seq[File])

object FileListReader {
  val default: FileListReader = (f: File) => {
    assert(f.exists(), s"$f should refer to an existing file or directory")
    val xmlFiles: Seq[File] = if (f.isDirectory) {
      val matchedFiles = f.listFiles(_.getName.isXmlType)
      matchedFiles.toList
    } else if (f.getName.isXmlType)
      Seq(f)
    else
      Seq.empty
    xmlFiles
  }

  extension (fn: String) {
    def isXmlType: Boolean = {
      val reqXmlPattern = ".+-req\\.xml"
      val respXmlPattern = ".+-rsp\\.xml"
      fn.matches(s"($reqXmlPattern)|($respXmlPattern)")
    }
  }
}
