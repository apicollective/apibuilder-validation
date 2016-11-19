package io.flow.lib.apidoc.json.validation

import com.bryzek.apidoc.spec.v0.models.{Method, Service}
import com.bryzek.apidoc.spec.v0.models.json._
import java.net.URL

import play.api.libs.json._

import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
  * Wraps a single apidoc service, providing helpers to validate
  * objects based on the incoming http method and path
  */
case class ApidocService(
  service: Service
) {

  private[this] val typeLookup = TypeLookup(service)
  private[this] val validator = JsonValidator(service)

  def typeFromPath(method: String, path: String): Option[String] = {
    typeLookup.typeFromPath(method = method, path = path)
  }

  def validate(method: String, path: String, js: JsValue): Either[Seq[String], JsValue] = {
    val methods = typeLookup.operationsByMethod(path)
    methods.get(Method(method)) match {
      case None => {
        methods.keys.toList match {
          case Nil => {
            Left(Seq(s"HTTP method $method not supported for path $path"))
          }

          case available => {
            Left(Seq(s"HTTP method $method not supported for path $path - Available methods: " + available.map(_.toString).mkString(", ")))
          }
        }
      }

      case Some(op) => {
        op.body.map(_.`type`) match {
          case None => Right(js)
          case Some(typ) => validator.validate(typ, js)
        }
      }
    }
  }

}

object ApidocService {

  /**
    * Loads the apidoc service specification from the specified URI,
    * returning either a list of errors or the service itself.
    */
  def fromUrl(url: String): Either[Seq[String], ApidocService] = {
    Try {
      Source.fromURL(new URL(url),  "UTF-8").mkString
    } match {
      case Success(contents) => {
        toService(contents)
      }
      case Failure(ex) => Left(Seq(s"Error downloading url[$url]: ${ex.getMessage}"))
    }
  }

  def toService(contents: String): Either[Seq[String], ApidocService] = {
    Json.parse(contents).validate[Service] match {
      case s: JsSuccess[Service] => Right(ApidocService(s.get))
      case e: JsError => Left(Seq(s"Error parsing service: $e"))
    }
  }
  
}
