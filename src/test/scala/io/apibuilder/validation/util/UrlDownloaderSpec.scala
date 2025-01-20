package io.apibuilder.validation.util

import cats.implicits._
import io.apibuilder.helpers.{FileHelpers, TestHelpers}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class UrlDownloaderSpec extends AnyWordSpec with Matchers with FileHelpers with TestHelpers {
  "java.net.URL" must {
    "handles the happy path" in {
      val urlStr = writeToTempFile("").toURI.toURL
      expectValid {
        UrlDownloader.withInputStream(urlStr)(_ => ().validNec)
      }
    }

    "handles the error case" in {
      val urlStr = writeToTempFile("").toURI.toURL
      expectInvalidNec {
        UrlDownloader.withInputStream(urlStr)(_ => "".invalidNec)
      }
    }

    "handles the empty error case" in {
      val urlStr = writeToTempFile("").toURI.toURL
      expectInvalidNec {
        UrlDownloader.withInputStream(urlStr)(_ => "".invalidNec)
      }
    }
  }

  "Url String" must {
    "handles the happy path" in {
      val urlStr = writeToTempFile("").toURI.toURL.toString
      expectValidNec {
        UrlDownloader.withInputStream(urlStr)(_ => ().validNec)
      }
    }

    "handles the invalid URL case" in {
      expectInvalidNec {
        UrlDownloader.withInputStream("boom")(_ => ().validNec)
      } mustBe Seq("Invalid URL: no protocol: boom")
    }

    "handles the error case" in {
      val urlStr = writeToTempFile("").toURI.toURL.toString
      expectInvalidNec {
        UrlDownloader.withInputStream(urlStr)(_ => "".invalidNec)
      }
    }

    "handles the empty error case" in {
      val urlStr = writeToTempFile("").toURI.toURL.toString
      expectInvalidNec {
        UrlDownloader.withInputStream(urlStr)(_ => "".invalidNec)
      }
    }
  }
}
