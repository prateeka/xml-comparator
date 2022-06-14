package com.prateek.xmlcompare

import scala.xml.{Node, Utility}

import java.io.File

import com.prateek.xmlcompare.read.{trim, Valid}

given stringToFile: Conversion[String, File] = new File(_)
