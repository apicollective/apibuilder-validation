package io.apibuilder.validation

import io.apibuilder.spec.v0.models._
import io.apibuilder.validation.util.StandardErrors
import io.apibuilder.validation.zip.ZipFileReader
import play.api.libs.json._

/**
  * Wrapper to work with multiple API Builder services.
  * Takes an ordered list of services. If multiple
  * services define an http path, first one is selected.
  */
case class MultiService(
  services: Seq[ApiBuilderService]
) {

  private[this] val validator = JsonValidator(services.map(_.service))

  def findType(name: String): Seq[ApibuilderType] = validator.findType(name, defaultNamespace = None)

  def findType(namespace: String, name: String): Seq[ApibuilderType] = validator.findType(namespace, name)

  /**
    * If the specified method & path requires a body, returns the type of the body
    */
  def bodyTypeFromPath(method: String, path: String): Option[String] = {
    resolveService(method, path) match {
      case Left(_) => None
      case Right(service) => service.bodyTypeFromPath(method, path)
    }
  }

  /**
    * For the given method & path, returns the defined operation, if any
    */
  def operation(method: String, path: String): Option[Operation] = {
    resolveService(method, path) match {
      case Left(_) => None
      case Right(service) => {
        service.operation(method = method, path = path)
      }
    }
  }

  /**
    * Validates the js value across all services, upcasting types to
    * match the request method/path as needed.
    */
  def upcast(method: String, path: String, js: JsValue): Either[Seq[String], JsValue] = {
    resolveService(method, path) match {
      case Left(errors) => {
        Left(errors)
      }
      case Right(service) => {
        service.validate(method = method, path = path) match {
          case Left(errors) => {
            Left(errors)
          }
          case Right(op) => {
            op.body.map(_.`type`) match {
              case None => {
                Right(js)
              }
              case Some(typeName) => {
                service.findType(typeName) match {
                  case None => upcast(typeName, js)
                  case Some(typ) => upcast(typ, js)
                }
              }
            }
          }
        }
      }
    }
  }

  /**
    * Upcast the json value based on the specified type name
    *
    * @param typeName e.g. 'user' - looks up the apibuilder type with this name
    *                 and if found, uses that type to validate and upcase the
    *                 JSON. Note if the type is not found, the JSON returned
    *                 is unchanged.
    */
  def upcast(typeName: String, js: JsValue): Either[Seq[String], JsValue] = {
    validator.validate(typeName, js, defaultNamespace = None)
  }

  def upcast(typ: ApibuilderType, js: JsValue): Either[Seq[String], JsValue] = {
    validator.validateType(typ, js)
  }

  /**
   * Validates that the path is known and the method is supported for the path.
   * If known, returns the corresponding operation. Otherwise returns a
   * list of errors.
   */
  def validate(method: String, path: String): Either[Seq[String], Operation] = {
    resolveService(method, path) match {
      case Left(errors) => Left(errors)
      case Right(service) => service.validate(method, path)
    }
  }

  def validate(
    typ: ApibuilderType,
    js: JsValue,
    prefix: Option[String] = None
  ): Either[Seq[String], JsValue] = {
    validator.validateType(typ, js, prefix)
  }

  /**
   * Looks up the response for the given status code for this operation, or None
   * if there is no response documented for the status code
   */
  def response(operation: Operation, responseCode: Int): Option[Response] = {
    operation.responses.find { r =>
      r.code match {
        case ResponseCodeInt(s) => s == responseCode
        case _ => false
      }
    } match {
      case Some(r) => {
        Some(r)
      }

      case None => {
        operation.responses.find { r =>
          r.code match {
            case ResponseCodeOption.Default => true
            case _ => false
          }
        }
      }
    }
  }

  /**
    * If the responseCode is valid for the operation, returns a Right(Unit) - otherwise
    * returns an error message detailing the difference in expectation.
    */
  def validateResponseCode(operation: Operation, responseCode: Int): Either[String, Response] = {
    response(operation, responseCode) match {
      case Some(r) => {
        Right(r)
      }

      case None => {
        Left(
          s"Unexpected response code[$responseCode] for operation[${operation.method} ${operation.path}]. Declared response codes: " +
            declaredResponseCodes(operation).mkString(", ")
        )
      }
    }
  }

  /**
    * Returns a list of the declared response codes. If ALL response codes are valid,
    * will return ["*"]
    */
  private[this] def declaredResponseCodes(operation: Operation): Seq[String] = {
    val responseCodes = operation.responses.map(_.code)
    if (responseCodes.exists {
      case ResponseCodeOption.Default => true
      case _ => false
    }) {
      // All response codes are valid
      Seq("*")

    } else {
      responseCodes.flatMap {
        case ResponseCodeOption.Default => None
        case ResponseCodeOption.UNDEFINED(_) => None
        case ResponseCodeInt(value) => Some(value.toString)
        case ResponseCodeUndefinedType(_) => None
      }
    }
  }

  /**
    * resolve the API Builder service defined at the provided method, path.
    * if no service, return a nice error message. Otherwise invoke
    * the provided function on the API Builder service.
    */
  private[validation] def resolveService(method: String, path: String): Either[Seq[String], ApiBuilderService] = {
    Method.fromString(method) match {
      case None => Left(Seq(StandardErrors.invalidMethodError(method)))
      case Some(m) => resolveService(m, path)
    }
  }

  private[this] def resolveService(method: Method, path: String): Either[Seq[String], ApiBuilderService] = {
    services.filter { s =>
      s.isDefinedAt(method = method, path = path)
    } match {
      case Nil => {
        services.find(_.isPathDefinedAt(path)) match {
          case None => {
            Left(Seq(s"HTTP path '$path' is not defined"))
          }

          case Some(s) => s.validate(method, path) match {
            case Left(errors) => Left(errors)
            case Right(_) => Right(s)
          }
        }
      }
      case one :: Nil => Right(one)

      case multiple => {
        // If we find a non dynamic path in any service, return that one.
        // Otherwise return the first matching service. This handles ambiguity:
        //   - service 1 defines POST /:organization/tokens
        //   - service 2 defines POST /users/tokens
        // We want to return service 2 when the path is /users/tokens
        Right(
          multiple.find { s =>
            s.validate(method, path) match {
              case Right(op) if Route.isStatic(op.path) => true
              case _ => false
            }
          }.getOrElse {
            multiple.head
          }
        )
      }
    }
  }

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
          MultiService(
            services = eithers.map(_.right.get)
          )
        )
      }
      case errors => Left(errors)
    }
  }

}
