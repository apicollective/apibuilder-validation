package io.apibuilder.validation.helpers

import java.io.{File, PrintWriter}

import scala.io.Source

trait FileHelpers {

  def readFileAsString(file: File): String = {
    val source = Source.fromFile(file,  "UTF-8")
    try {
      source.mkString("")
    } finally {
      source.close()
    }
  }

  def writeToTempFile(
    contents: String,
    prefix: String = "apibuildervalidation",
    suffix: String = "tmp"
  ): File = {
    val tmp = File.createTempFile(prefix, s".$suffix")
    tmp.deleteOnExit()
    new PrintWriter(tmp) {
      try {
        write(contents)
      } finally {
        close()
      }
    }
    tmp
  }
}

