package io.apibuilder.validation.util

import io.apibuilder.validation.helpers
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec

class UrlDownloaderSpec extends AnyFunSpec with Matchers with helpers.FileHelpers {
  describe("java.net.URL") {
    it("handles the happy path") {
      val urlStr = writeToTempFile("").toURI.toURL
      UrlDownloader.withInputStream(urlStr)(_ => Right("yeah")) shouldBe Right("yeah")
    }

    it("handles the error case") {
      val urlStr = writeToTempFile("").toURI.toURL
      UrlDownloader.withInputStream(urlStr)(_ => Left(Seq("boo"))) shouldBe Left(Seq("boo"))
    }

    it("handles the empty error case") {
      val urlStr = writeToTempFile("").toURI.toURL
      UrlDownloader.withInputStream(urlStr)(_ => Left(Nil)) shouldBe Left(Nil)
    }
  }

  describe("Url String") {
    it("handles the happy path") {
      val urlStr = writeToTempFile("").toURI.toURL.toString
      UrlDownloader.withInputStream(urlStr)(_ => Right("yeah")) shouldBe Right("yeah")
    }

    it("handles the invalid URL case") {
      UrlDownloader.withInputStream("boom")(_ => Right("unused")) shouldBe Left(Seq("Invalid URL: no protocol: boom"))
    }

    it("handles the error case") {
      val urlStr = writeToTempFile("").toURI.toURL.toString
      UrlDownloader.withInputStream(urlStr)(_ => Left(Seq("boo"))) shouldBe Left(Seq("boo"))
    }

    it("handles the empty error case") {
      val urlStr = writeToTempFile("").toURI.toURL.toString
      UrlDownloader.withInputStream(urlStr)(_ => Left(Nil)) shouldBe Left(Nil)
    }
  }
}
