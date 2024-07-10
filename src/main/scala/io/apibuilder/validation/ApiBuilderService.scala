package io.apibuilder.validation

import cats.implicits._
import cats.data.ValidatedNec

import java.io.{BufferedInputStream, ByteArrayOutputStream, File, FileInputStream, InputStream}
import io.apibuilder.spec.v0.models.{Method, Operation, Service}
import io.apibuilder.spec.v0.models.json.*

import java.nio.charset.StandardCharsets
import io.apibuilder.validation.util.UrlDownloader
import play.api.libs.json.*

/**
  * Wraps a single API Builder service, providing helpers to validate
  * objects based on the incoming http method and path
  */
case class ApiBuilderService(
  service: Service
) {
  private lazy val validator = JsonValidator(this)
  private val normalizer = PathNormalizer(service)

  val name: String = service.name
  val namespace: String = service.namespace

  final lazy val enums: Seq[ApiBuilderType.Enum] = service.enums.map { e => ApiBuilderType.Enum(this, e) }
  final lazy val interfaces: Seq[ApiBuilderType.Interface] = service.interfaces.map { m => ApiBuilderType.Interface(this, m) }
  final lazy val models: Seq[ApiBuilderType.Model] = service.models.map { m => ApiBuilderType.Model(this, m) }
  final lazy val unions: Seq[ApiBuilderType.Union] = service.unions.map { u => ApiBuilderType.Union(this, u) }

  final lazy val allTypes: Seq[ApiBuilderType] = enums ++ interfaces ++ models ++ unions

  def findType(name: String): Option[AnyType] = {
    validator.findType(name, defaultNamespace = Some(service.namespace)).headOption
  }

  def findOperation(method: Method, path: String): Option[Operation] = {
    normalizer.resolve(method, path).toOption
  }

}

object ApiBuilderService {

  /**
    * Loads the API Builder service specification from the specified URI,
    * returning either a list of errors or the service itself.
    */
  def fromUrl(url: String): ValidatedNec[String, ApiBuilderService] = {
    UrlDownloader.withInputStream(url) { is =>
      fromInputStream(is)
    }
  }

  def fromFile(file: File): ValidatedNec[String, ApiBuilderService] = {
    val is = new BufferedInputStream(new FileInputStream(file))
    try {
      fromInputStream(is)
    } finally {
      is.close()
    }
  }

  def fromInputStream(inputStream: InputStream): ValidatedNec[String, ApiBuilderService] = {
    toService(copyToString(inputStream))
  }

  private def copyToString(inputStream: InputStream): String = {
    val result = new ByteArrayOutputStream()
    val buffer = new Array[Byte](1024)
    var length = inputStream.read(buffer)
    while (length != -1) {
      result.write(buffer, 0, length)
      length = inputStream.read(buffer)
    }
    result.toString(StandardCharsets.UTF_8.name())
  }

  def toService(contents: String): ValidatedNec[String, ApiBuilderService] = {
    Json.parse(contents).validate[Service] match {
      case JsSuccess(s: Service, _) => ApiBuilderService(s).validNec
      case e: JsError => s"Error parsing service: $e".invalidNec
    }
  }

}
