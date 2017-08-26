package io.apibuilder.validation

import io.apibuilder.spec.v0.models.{Method, Operation, Parameter, Service}
import io.apibuilder.spec.v0.models.json._
import java.net.URL

import play.api.libs.json._

import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
  * Wraps a single API Builder service, providing helpers to validate
  * objects based on the incoming http method and path
  */
case class ApiBuilderService(
  service: Service
) {

  private[this] val normalizer = PathNormalizer(service)

  private[this] val validator = JsonValidator(service)

  /**
    * If the specified method, path require a body, returns the type of the body
    */
  def bodyTypeFromPath(method: String, path: String): Option[String] = {
    operation(method, path).flatMap(_.body.map(_.`type`))
  }

  /**
    * If the specified method, path exists, returns the list of parameters it accepts
    */
  def parametersFromPath(method: String, path: String): Option[Seq[Parameter]] = {
    operation(method, path).map(_.parameters)
  }

  /**
    * Returns a map of the HTTP Methods available for the given path
    */
  def methodsFromPath(path: String): Seq[Method] = {
    Method.all.flatMap { m =>
      normalizer.resolve(m, path)
    }.map(_.method)
  }

  def isDefinedAt(method: String, path: String): Boolean = {
    validate(method, path).isRight
  }

  def isPathDefinedAt(path: String): Boolean = {
    Method.all.exists { m =>
      normalizer.resolve(m, path).nonEmpty
    }
  }

  /**
    * If the provided method and path are known, returns the associated
    * operation. Otherwise returns an appropriate error message.
    */
  def validate(method: String, path: String): Either[Seq[String], Operation] = {
    normalizer.resolve(method, path) match {
      case Left(errors) => Left(errors)
      case Right(result) => {
        result match {
          case Some(op) => Right(op)
          case None => {
            methodsFromPath(path).toList match {
              case Nil => {
                Left(Seq(s"HTTP path $path is not defined"))
              }

              case available => {
                Left(Seq(s"HTTP method '$method' not supported for path $path - Available methods: " + available.map(_.toString).mkString(", ")))
              }
            }
          }
        }
      }
    }
  }

  /**
    * Returns the operation associated with the specified method and path, if any
    */
  def operation(method: String, path: String): Option[Operation] = {
    Method.fromString(method) match {
      case None => None
      case Some(m) => normalizer.resolve(m, path)
    }
  }

  def upcast(method: String, path: String, js: JsValue): Either[Seq[String], JsValue] = {
    validate(method = method, path = path) match {
      case Left(errors) => Left(errors)
      case Right(op) => {
        op.body.map(_.`type`) match {
          case None => Right(js)
          case Some(typ) => validator.validate(typ, js)
        }
      }
    }
  }

}

object ApiBuilderService {

  /**
    * Loads the API Builder service specification from the specified URI,
    * returning either a list of errors or the service itself.
    */
  def fromUrl(url: String): Either[Seq[String], ApiBuilderService] = {
    Try {
      Source.fromURL(new URL(url),  "UTF-8").mkString
    } match {
      case Success(contents) => {
        toService(contents)
      }
      case Failure(ex) => Left(Seq(s"Error downloading url[$url]: ${ex.getMessage}"))
    }
  }

  def toService(contents: String): Either[Seq[String], ApiBuilderService] = {
    Json.parse(contents).validate[Service] match {
      case s: JsSuccess[Service] => Right(ApiBuilderService(s.get))
      case e: JsError => Left(Seq(s"Error parsing service: $e"))
    }
  }
  
}
