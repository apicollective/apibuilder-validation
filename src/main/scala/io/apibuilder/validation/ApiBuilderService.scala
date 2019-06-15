package io.apibuilder.validation

import java.io.{BufferedInputStream, ByteArrayOutputStream, File, FileInputStream, InputStream}

import io.apibuilder.spec.v0.models.{Method, Operation, Service}
import io.apibuilder.spec.v0.models.json._
import java.nio.charset.StandardCharsets

import io.apibuilder.validation.util.UrlDownloader
import play.api.libs.json._

/**
  * Wraps a single API Builder service, providing helpers to validate
  * objects based on the incoming http method and path
  */
case class ApiBuilderService(
  service: Service
) {
  private[this] val validator = JsonValidator(service)
  private[this] val normalizer = PathNormalizer(service)

  val name: String = service.name
  val namespace: String = service.namespace

  def findType(name: String): Option[ApiBuilderType] = {
    validator.findType(name, defaultNamespace = Some(service.namespace)).headOption
  }

  def findOperation(method: Method, path: String): Option[Operation] = {
    normalizer.resolve(method, path) match {
      case Right(op) => Some(op)
      case Left(_) => None
    }
  }

}

object ApiBuilderService {

  /**
    * Loads the API Builder service specification from the specified URI,
    * returning either a list of errors or the service itself.
    */
  def fromUrl(url: String): Either[Seq[String], ApiBuilderService] = {
    UrlDownloader.withInputStream(url) { is =>
      fromInputStream(is)
    }
  }

  def fromFile(file: File): Either[Seq[String], ApiBuilderService] = {
    val is = new BufferedInputStream(new FileInputStream(file))
    try {
      fromInputStream(is)
    } finally {
      is.close()
    }
  }

  def fromInputStream(inputStream: InputStream): Either[Seq[String], ApiBuilderService] = {
    toService(copyToString(inputStream))
  }

  private[this] def copyToString(inputStream: InputStream): String = {
    val result = new ByteArrayOutputStream()
    val buffer = new Array[Byte](1024)
    var length = inputStream.read(buffer)
    while (length != -1) {
      result.write(buffer, 0, length)
      length = inputStream.read(buffer)
    }
    result.toString(StandardCharsets.UTF_8.name())
  }

  def toService(contents: String): Either[Seq[String], ApiBuilderService] = {
    Json.parse(contents).validate[Service] match {
      case s: JsSuccess[Service] => Right(ApiBuilderService(s.get))
      case e: JsError => Left(Seq(s"Error parsing service: $e"))
    }
  }
  
}
