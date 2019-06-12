package io.apibuilder.validation

import io.apibuilder.spec.v0.models._
import io.apibuilder.validation.zip.ZipFileReader
import play.api.libs.json._

/**
  * Wrapper to work with multiple API Builder services.
  * Takes an ordered list of services. If multiple
  * services define an http path, first one is selected.
  */
trait MultiService extends ResponseHelpers {
  def services: Seq[ApiBuilderService]

  def findType(name: String): Seq[ApibuilderType]

  def findType(namespace: String, name: String): Seq[ApibuilderType]

  /**
    * If the specified method & path requires a body, returns the type of the body
    */
  final def bodyTypeFromPath(method: String, path: String): Option[String] = {
    operation(method, path).flatMap(_.body.map(_.`type`))
  }

  /**
    * For the given method & path, returns the defined operation, if any
    */
  final def operation(method: String, path: String): Option[Operation] = {
    validate(method, path) match {
      case Left(_) => None
      case Right(op) => Some(op)
    }
  }

  /**
    * Validates the js value across all services, upcasting types to
    * match the request method/path as needed.
    */
  def upcast(method: String, path: String, js: JsValue): Either[Seq[String], JsValue]

  /**
    * Upcast the json value based on the specified type name
    *
    * @param typeName e.g. 'user' - looks up the apibuilder type with this name
    *                 and if found, uses that type to validate and upcase the
    *                 JSON. Note if the type is not found, the JSON returned
    *                 is unchanged.
    */
  def upcast(typeName: String, js: JsValue): Either[Seq[String], JsValue]

  def upcast(typ: ApibuilderType, js: JsValue): Either[Seq[String], JsValue]

  /**
   * Validates that the path is known and the method is supported for the path.
   * If known, returns the corresponding operation. Otherwise returns a
   * list of errors.
   */
  def validate(method: String, path: String): Either[Seq[String], Operation]

  def validate(typ: ApibuilderType, js: JsValue, prefix: Option[String] = None): Either[Seq[String], JsValue]

}

object MultiService {

  def fromUrl(url: String): Either[Seq[String], MultiService] = {
    fromUrls(urls = Seq(url))
  }

  /**
  * Loads the list of API Builder service specification from the specified URIs,
  * returning either a list of errors or an instance of MultiService
  */
  def fromUrls(urls: Seq[String]): Either[Seq[String], MultiService] = {
    val eithers = urls.flatMap { url =>
      if (ZipFileReader.isZipFile(url)) {
        ZipFileReader.fromUrl(url) match {
          case Left(errors) => Seq(Left(errors))
          case Right(reader) => {
            reader.entries.map { e =>
              ApiBuilderService.fromFile(e.file)
            }
          }
        }
      } else {
        Seq(ApiBuilderService.fromUrl(url))
      }
    }

    eithers.flatMap(_.left.getOrElse(Nil)).toList match {
      case Nil => {
        Right(
          MultiServiceImpl(
            services = eithers.map(_.right.get)
          )
        )
      }
      case errors => Left(errors)
    }
  }

}
