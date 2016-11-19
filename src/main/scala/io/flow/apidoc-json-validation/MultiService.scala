package io.flow.lib.apidoc.json.validation

import com.bryzek.apidoc.spec.v0.models.{Operation, Service}
import play.api.libs.json._

/**
  * Wrapper to work with multiple apidoc services.
  * Takes an ordered list of services. If multiple
  * services define an http path, first one is selected.
  */
case class MultiService(
  services: Seq[ApidocService]
) {

  /**
    * If the specified method, path require a body, returns the type of the body
    */
  def bodyTypeFromPath(method: String, path: String): Option[String] = {
    services.flatMap(_.bodyTypeFromPath(method, path)).headOption
  }

  /**
    * Validates the js value across all services, upcasting types to
    * match the request method/path as needed.
    */
  def upcast(method: String, path: String, js: JsValue): Either[Seq[String], JsValue] = {
    val start: Either[Seq[String], JsValue] = Right(js)
    services.foldLeft(start) { case (result, service) =>
      result match {
        case Left(errors) => Left(errors)
        case Right(v) => service.upcast(method, path, v)
      }
    }
  }

  /**
    * Validates that the path is known and the method is supported for the path.
    * If known, returns the corresponding operation. Otherwise returns a
    * list of errors.
    */
  def validate(method: String, path: String): Either[Seq[String], Operation] = {
    services.find(_.isDefined(path)) match {
      case None => {
        Left(Seq(s"Unknown HTTP path $path"))
      }
      case Some(s) => {
        s.validate(method, path)
      }
    }
  }

}

object MultiService {
  
  /**
    * Loads the list of apidoc service specification from the specified URIs,
    * returning either a list of errors or an instance of MultiService
    */
  def fromUrls(urls: Seq[String]): Either[Seq[String], MultiService] = {
    val eithers = urls.map { ApidocService.fromUrl(_) }
    eithers.forall(_.isRight) match {
      case true => {
        Right(
          MultiService(
            services = eithers.map(_.right.get)
          )
        )
      }

      case false => {
        Left(eithers.map(_.left.getOrElse(Nil)).flatten)
      }
    }
  }

}
