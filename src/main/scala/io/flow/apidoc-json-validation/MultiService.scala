package io.flow.lib.apidoc.json.validation

import com.bryzek.apidoc.spec.v0.models.{Method, Operation, Parameter, Service}
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
    * resolve the apidoc service defined at the provided method, path.
    * if no service, return a nice error message. Otherwise invoke
    * the provided function on the apidoc service.
    */
  private[this] def resolveService(method: String, path: String): Either[Seq[String], ApidocService] = {
    services.find { s =>
      s.isDefinedAt(method = method, path = path)
    } match {
      case Some(s) => Right(s)
      case None => {
        services.find(_.isPathDefinedAt(path)) match {
          case None => Left(Seq(s"HTTP '$method $path' is not defined"))
          case Some(s) => s.validate(method, path) match {
            case Left(errors) => Left(errors)
            case Right(_) => Right(s)
          }
        }
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
    val eithers = urls.map { ApidocService.fromUrl }
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
