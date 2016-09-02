package io.flow.lib.apidoc.json.validation

import com.bryzek.apidoc.spec.v0.models.Service
import com.bryzek.apidoc.spec.v0.models.json._
import java.net.URL
import play.api.libs.json._
import scala.io.Source
import scala.util.{Failure, Success, Try}

case class ApidocService(
  uri: String,
  service: Service
)

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
        Json.parse(contents).validate[Service] match {
          case s: JsSuccess[Service] => Right(ApidocService(url, s.get))
          case e: JsError => Left(Seq(s"Error parsing service from url[$url]: $e"))
        }
      }
      case Failure(ex) => Left(Seq(s"Error downloading url[$url]: ${ex.getMessage}"))
    }
  }

}
