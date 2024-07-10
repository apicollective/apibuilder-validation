package io.apibuilder.validation.util

import io.apibuilder.helpers.FileHelpers
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FileOrderSpec extends AnyWordSpec with Matchers
  with FileHelpers
{
  "sortOrder with no ordering file is alphabetical" in {
    FileOrder(None).sort(Seq("b", "a")) must equal(
      List("a", "b")
    )
  }

  "sortOrder respects ordering file" in {
    FileOrder(
      Some(writeToTempFile(contents = "b\na"))
    ).sort(Seq("b", "a")) must equal(
      List("b", "a")
    )
  }

  "sortOrder respects ordering file for entries in it, alphabetical for rest" in {
    FileOrder(
      Some(writeToTempFile(contents = "c"))
    ).sort(Seq("c", "b", "a")) must equal(
      List("c", "a", "b")
    )
  }

}