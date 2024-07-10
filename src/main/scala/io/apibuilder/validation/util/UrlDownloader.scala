package io.apibuilder.validation.util

import java.io.{BufferedInputStream, InputStream}
import java.net.URL

import cats.data.ValidatedNec
import cats.implicits._

import scala.util.{Failure, Success, Try}

object ValidatedUrlDownloader {
  def validateUrl(url: String): ValidatedNec[String, URL] = {
    Try {
      new URL(url)
    } match {
      case Success(u) => u.validNec
      case Failure(ex) => s"Invalid URL: ${ex.getMessage}".invalidNec
    }
  }

  def withInputStream[T](url: String)(f: InputStream => ValidatedNec[String, T]): ValidatedNec[String, T] = {
    validateUrl(url).andThen(withInputStream(_)(f))
  }

  def withInputStream[T](url: URL)(f: InputStream => ValidatedNec[String, T]): ValidatedNec[String, T] = {
    withInputStream(url, f, _.invalidNec)
  }

  private[util] def withInputStream[T](url: URL, f: InputStream => T, e: String => T): T = {
    Try {
      val connection = url.openConnection()
      // add a request property to allow access to the net
      connection.addRequestProperty("User-Agent", "Mozilla/4.76")
      val is = new BufferedInputStream(connection.getInputStream)
      try {
        f(is)
      } finally {
        is.close()
      }
    } match {
      case Success(r) => r
      case Failure(ex) => e(s"Error downloading URL '$url': ${ex.getMessage}")
    }
  }
}

object UrlDownloader {
  import io.apibuilder.validation.util.Implicits._

  def withInputStream[T](url: String)(f: InputStream => ValidatedNec[String, T]): ValidatedNec[String, T] =
    ValidatedUrlDownloader.validateUrl(url).toEither.leftToSeq.flatMap(ValidatedUrlDownloader.withInputStream(_, f, e => Left(Seq(e))))

  def withInputStream[T](url: URL)(f: InputStream => ValidatedNec[String, T]): ValidatedNec[String, T] =
    ValidatedUrlDownloader.withInputStream(url, f, e => Left(Seq(e)))
}