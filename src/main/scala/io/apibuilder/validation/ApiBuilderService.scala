package io.apibuilder.validation

import java.io.File

import io.apibuilder.spec.v0.models.{Method, Operation, Service}
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
  private[this] val validator = JsonValidator(service)
  private[this] val normalizer = PathNormalizer(service)

  /**
    * If the specified method, path require a body, returns the type of the body
    */
  def bodyTypeFromPath(method: String, path: String): Option[String] = {
    operation(method, path).flatMap(_.body.map(_.`type`))
  }

  def findType(name: String): Option[ApibuilderType] = {
    validator.findType(name, defaultNamespace = Some(service.namespace)).headOption
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

}

object ApiBuilderService {

  /**
    * Loads the API Builder service specification from the specified URI,
    * returning either a list of errors or the service itself.
    */
  def fromUrl(url: String): Either[Seq[String], ApiBuilderService] = {
    val source = Source.fromURL(new URL(url),  "UTF-8")
    try {
      fromSource(source)
    } finally {
      source.close()
    }
  }

  def fromFile(file: File): Either[Seq[String], ApiBuilderService] = {
    val source = Source.fromFile(file,  "UTF-8")
    try {
      fromSource(source)
    } finally {
      source.close()
    }
  }

  def fromSource(source: Source): Either[Seq[String], ApiBuilderService] = {
    Try {
      source.mkString
    } match {
      case Success(contents) => toService(contents)
      case Failure(ex) => Left(Seq(s"Error creating ApiBuilderService: ${ex.getMessage}"))
    }
  }

  def toService(contents: String): Either[Seq[String], ApiBuilderService] = {
    Json.parse(contents).validate[Service] match {
      case s: JsSuccess[Service] => Right(ApiBuilderService(s.get))
      case e: JsError => Left(Seq(s"Error parsing service: $e"))
    }
  }
  
}
