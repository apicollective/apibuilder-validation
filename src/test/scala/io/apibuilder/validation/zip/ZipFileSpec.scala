package io.apibuilder.validation.zip

import java.io.File
import io.apibuilder.validation.helpers
import io.apibuilder.validation.helpers.FileHelpers
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ZipFileSpec extends AnyWordSpec with Matchers with FileHelpers {
  "isZipFile" in {
    ZipFileReader.isZipFile("foo.zip") mustBe true
    ZipFileReader.isZipFile("foo.ZIP") mustBe true
    ZipFileReader.isZipFile(" foo.zip ") mustBe true
    ZipFileReader.isZipFile(" foo.csv ") mustBe false
  }

  "isJsonFile" in {
    ZipFileReader.isJsonFile("foo.json") mustBe true
    ZipFileReader.isJsonFile("foo.JSON") mustBe true
    ZipFileReader.isJsonFile(" foo.json ") mustBe true
    ZipFileReader.isJsonFile(" foo.csv ") mustBe false
  }

  "withFile defaults to file name" in {
    val tmp = File.createTempFile("foo", "json")
    val zip = ZipFileBuilder().withFile(tmp).build()
    ZipFileReader.fromFile(zip).entries.map(_.name) must equal(
      Seq(tmp.getName)
    )
  }

  "withFile respects given name" in {
    val tmp = File.createTempFile("foo", "json")
    val zip = ZipFileBuilder().withFile("bar.json", tmp).build()
    ZipFileReader.fromFile(zip).entries.map(_.name) must equal(
      Seq("bar.json")
    )
  }

  "withTextFile" in {
    val zip = ZipFileBuilder().withTextFile("order.txt", "a\nb").build()
    val reader = ZipFileReader.fromFile(zip)
    reader.entries.map(_.name) must equal(
      Seq("order.txt")
    )
    readFileAsString(reader.entries.head.file).split("\n") must equal(Seq("a", "b"))
  }

  "files are preserved" in {
    val zip = ZipFileBuilder()
      .withFile("foo.json", writeToTempFile("foo"))
      .withFile("bar.json", writeToTempFile("bar"))
      .build()
    val entries = ZipFileReader.fromFile(zip).entries
    entries.map(_.name).sorted must equal(
      Seq("bar.json", "foo.json")
    )
    def read(name: String): String = {
      readFileAsString(entries.find(_.name == name).get.file)
    }
    read("bar.json") must equal("bar")
    read("foo.json") must equal("foo")
  }
}