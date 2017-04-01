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
    services.flatMap(_.bodyTypeFromPath(method, path)).headOption
  }

  /**
    * For the given method & path, returns a list of the defined parameters
    */
  def parametersFromPath(method: String, path: String): Option[Seq[Parameter]] = {
    services.flatMap(_.parametersFromPath(method, path)).headOption
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
println("STARTING VALIDATION")
    val validations = services.map(_.validate(method, path))
    validations.foreach { v =>
      println(v)
    }

    validations.find(_.isRight) match {
      case Some(validation) => Right(validation.right.get)
      case None => validations.find(_.isLeft).getOrElse {
        Left(Seq(s"HTTP $method $path is not defined"))
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
