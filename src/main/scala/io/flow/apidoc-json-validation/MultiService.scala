package io.flow.lib.apidoc.json.validation

import com.bryzek.apidoc.spec.v0.models.Service
import com.bryzek.apidoc.spec.v0.models.json._
import java.net.URL
import play.api.libs.json._
import scala.io.Source
import scala.util.{Failure, Success, Try}

case class MultiService(
  services: Seq[ApidocService]
)

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
