package io.apibuilder.validation.util

import java.io.{BufferedInputStream, InputStream}
import java.net.URL

import scala.util.{Failure, Success, Try}

object UrlDownloader {

  def withInputStream[T](url: String)(f: InputStream => Either[Seq[String], T]): Either[Seq[String], T] = {
    Try {
      new URL(url)
    } match {
      case Success(u) => withInputStream(u)(f)
      case Failure(ex) => Left(Seq(s"Invalid URL: ${ex.getMessage}"))
    }
  }

  def withInputStream[T](url: URL)(f: InputStream => Either[Seq[String], T]): Either[Seq[String], T] = {
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
      case Failure(ex) => Left(Seq(s"Error downloading URL '$url': ${ex.getMessage}"))
    }
  }
}