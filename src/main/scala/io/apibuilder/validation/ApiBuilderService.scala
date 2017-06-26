package io.apibuilder.validation

import io.apibuilder.spec.v0.models.{Method, Operation, Parameter, Service}
import io.apibuilder.spec.v0.models.json._
import java.net.URL

import play.api.libs.json._

import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
  * Wraps a single apidoc service, providing helpers to validate
  * objects based on the incoming http method and path
  */
case class ApiBuilderService(
  service: Service
) {

  private[this] val validator = JsonValidator(service)

  private[this] val byPaths: Map[String, Map[Method, Operation]] = {
    val tmp = scala.collection.mutable.Map[String, scala.collection.mutable.Map[Method, Operation]]()

    service.resources.flatMap(_.operations).map { op =>
      val canonicalPath = PathParser.parse(op.path).canonical

      val m = tmp.get(canonicalPath).getOrElse {
        val map = scala.collection.mutable.Map[Method, Operation]()
        tmp += canonicalPath -> map
        map
      }

      m += op.method -> op
    }
    tmp.map {
      case (canonicalPath, methods) => canonicalPath -> methods.toMap
    }.toMap
  }

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
    * Returns a map of the operations available for the specified path. Keys are the HTTP Methods.
    */
  def operationsByMethod(path: String): Map[Method, Operation] = {
    byPaths.getOrElse(PathParser.parse(path).canonical, Map.empty)
  }

  def isDefinedAt(method: String, path: String): Boolean = {
    validate(method, path).isRight
  }

  def isPathDefinedAt(path: String): Boolean = {
    byPaths.isDefinedAt(PathParser.parse(path).canonical)
  }
  /**
    * If the provided method and path are known, returns the associated
    * operation. Otherwise returns an appropriate error message.
    */
  def validate(method: String, path: String): Either[Seq[String], Operation] = {
    Method(method) match {
      case Method.UNDEFINED(_) => {
        Left(Seq(s"HTTP method '$method' is invalid. Must be one of: " + Method.all.map(_.toString).mkString(", ")))
      }

      case m => {
        val methods = operationsByMethod(path)
        methods.get(m) match {
          case Some(op) => Right(op)

          case None => {
            methods.keys.toList match {
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
    operationsByMethod(path).get(Method(method))
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
    * Loads the apidoc service specification from the specified URI,
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
