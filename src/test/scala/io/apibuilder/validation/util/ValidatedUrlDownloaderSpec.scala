package io.apibuilder.validation.util

import cats.data.NonEmptyChain
import cats.data.Validated.{Invalid, Valid}
import cats.implicits.*
import io.apibuilder.helpers.FileHelpers
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ValidatedUrlDownloaderSpec extends AnyWordSpec with Matchers with FileHelpers {
  "java.net.URL" must {
    "handles the happy path" in {
      val urlStr = writeToTempFile("").toURI.toURL
      ValidatedUrlDownloader.withInputStream(urlStr)(_ => "yeah".validNec) mustBe Valid("yeah")
    }

    "handles the error case" in {
      val urlStr = writeToTempFile("").toURI.toURL
      ValidatedUrlDownloader.withInputStream(urlStr)(_ => "boo".invalidNec) mustBe Invalid(NonEmptyChain.one("boo"))
    }
  }

  "Url String" must {
    "handles the happy path" in {
      val urlStr = writeToTempFile("").toURI.toURL.toString
      ValidatedUrlDownloader.withInputStream(urlStr)(_ => "yeah".validNec) mustBe Valid("yeah")
    }

    "handles the invalid URL case" in {
      ValidatedUrlDownloader.withInputStream("boom")(_ => "unused".validNec) mustBe Invalid(NonEmptyChain.one("Invalid URL: no protocol: boom"))
    }

    "handles the error case" in {
      val urlStr = writeToTempFile("").toURI.toURL.toString
      ValidatedUrlDownloader.withInputStream(urlStr)(_ => "boo".invalidNec) mustBe Invalid(NonEmptyChain.one("boo"))
    }
  }
}
