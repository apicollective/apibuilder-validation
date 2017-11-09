package io.apibuilder.validation

import io.apibuilder.spec.v0.models._
import play.api.libs.json._

/**
  * Wrapper to work with multiple API Builder services.
  * Takes an ordered list of services. If multiple
  * services define an http path, first one is selected.
  */
case class MultiService(
  services: Seq[ApiBuilderService]
) {

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
    * For the given method & path, returns a list of the defined parameters
    */
  def parametersFromPath(method: String, path: String): Option[Seq[Parameter]] = {
    resolveService(method, path) match {
      case Left(_) => None
      case Right(service) => service.parametersFromPath(method, path)
    }
  }

  /**
    * Validates the js value across all services, upcasting types to
    * match the request method/path as needed.
    */
  def upcast(method: String, path: String, js: JsValue): Either[Seq[String], JsValue] = {
    resolveService(method, path) match {
      case Left(errors) => Left(errors)
      case Right(service) => service.upcast(method, path,js)
    }
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

  /**
    * If the responseCode is valid for the operation, returns a Right(Unit) - otherwise
    * returns an error message detailing the difference in expectation.
    */
  def validateResponseCode(op: Operation, responseCode: Int): Either[String, Unit] = {
    val responseCodes = op.responses.map(_.code)
    if (responseCodes.exists {
      case ResponseCodeOption.Default => true
      case _ => false
    }) {
      // All response codes are valid
      Right(())

    } else {
      val expectedResponseCodes = responseCodes.flatMap {
        case ResponseCodeOption.Default => None
        case ResponseCodeOption.UNDEFINED(_) => None
        case ResponseCodeInt(value) => Some(value)
        case ResponseCodeUndefinedType(_) => None
      }

      if (expectedResponseCodes.contains(responseCode)) {
        Right(())
      } else {
        Left(
          s"Unexpected response code[$responseCode] for operation[${op.method} ${op.path}]. Expected response codes: " +
            expectedResponseCodes.mkString(", ")
        )
      }
    }
  }

  /**
    * resolve the API Builder service defined at the provided method, path.
    * if no service, return a nice error message. Otherwise invoke
    * the provided function on the API Builder service.
    */
  private[validation] def resolveService(method: String, path: String): Either[Seq[String], ApiBuilderService] = {
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
  
  /**
    * Loads the list of API Builder service specification from the specified URIs,
    * returning either a list of errors or an instance of MultiService
    */
  def fromUrls(urls: Seq[String]): Either[Seq[String], MultiService] = {
    val eithers = urls.map { ApiBuilderService.fromUrl }
    if (eithers.forall(_.isRight)) {
      Right(
        MultiService(
          services = eithers.map(_.right.get)
        )
      )
    } else {
      Left(eithers.flatMap(_.left.getOrElse(Nil)))
    }
  }

}
