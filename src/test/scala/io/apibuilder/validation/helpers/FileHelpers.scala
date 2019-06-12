package io.apibuilder.validation.helpers

import java.io.{File, PrintWriter}

trait FileHelpers {

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

