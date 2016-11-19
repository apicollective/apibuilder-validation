package io.flow.lib.apidoc.json.validation

import com.bryzek.apidoc.spec.v0.models.Service
import play.api.libs.json._

/**
  * Wrapper to work with multiple apidoc services
  */
case class MultiService(
  services: Seq[ApidocService]
) {

  def bodyTypeFromPath(method: String, path: String): Option[String] = {
    services.flatMap(_.typeFromPath(method, path)).headOption
  }
  
  /**
    * Validates the js value across all services.
    */
  def validate(method: String, path: String, js: JsValue): Either[Seq[String], JsValue] = {
    val start: Either[Seq[String], JsValue] = Right(js)
    services.foldLeft(start) { case (result, service) =>
      result match {
        case Left(errors) => Left(errors)
        case Right(v) => service.validate(method, path, v)
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
          eithers.map(_.right.get).foldLeft(MultiService(services = Nil)) { case (multi, svc) =>
            MultiService(
              services = multi.services ++ Seq(svc)
            )
          }
        )
      }

      case false => {
        Left(eithers.filter(_.isLeft).map(_.left.get).flatten)
      }
    }
  }

}
