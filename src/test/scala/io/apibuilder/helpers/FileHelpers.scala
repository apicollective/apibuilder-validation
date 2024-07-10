package io.apibuilder.helpers

import io.apibuilder.validation.zip.FileUtil

import java.io.File

trait FileHelpers {

  def readFileAsString(file: File): String = FileUtil.readFileAsString(file)

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

