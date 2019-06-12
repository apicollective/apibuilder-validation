package io.apibuilder.validation.helpers

import java.io.File

import io.apibuilder.validation.zip.FileUtil

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
    FileUtil.writeToTempFile(
      contents = contents,
      prefix = prefix,
      suffix = suffix
    )
  }
}

