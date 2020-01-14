package io.apibuilder.validation.util

import cats.data.NonEmptyChain
import cats.data.Validated.{Invalid, Valid}
import cats.implicits._
import io.apibuilder.validation.helpers
import org.scalatest.{FunSpec, Matchers}

class ValidatedUrlDownloaderSpec extends FunSpec with Matchers with helpers.FileHelpers {
  describe("java.net.URL") {
    it("handles the happy path") {
      val urlStr = writeToTempFile("").toURI.toURL
      ValidatedUrlDownloader.withInputStream(urlStr)(_ => "yeah".validNec) shouldBe Valid("yeah")
    }

    it("handles the error case") {
      val urlStr = writeToTempFile("").toURI.toURL
      ValidatedUrlDownloader.withInputStream(urlStr)(_ => "boo".invalidNec) shouldBe Invalid(NonEmptyChain.one("boo"))
    }
  }

  describe("Url String") {
    it("handles the happy path") {
      val urlStr = writeToTempFile("").toURI.toURL.toString
      ValidatedUrlDownloader.withInputStream(urlStr)(_ => "yeah".validNec) shouldBe Valid("yeah")
    }

    it("handles the invalid URL case") {
      ValidatedUrlDownloader.withInputStream("boom")(_ => "unused".validNec) shouldBe Invalid(NonEmptyChain.one("Invalid URL: no protocol: boom"))
    }

    it("handles the error case") {
      val urlStr = writeToTempFile("").toURI.toURL.toString
      ValidatedUrlDownloader.withInputStream(urlStr)(_ => "boo".invalidNec) shouldBe Invalid(NonEmptyChain.one("boo"))
    }
  }
}
