package io.apibuilder.validation

import java.net.{HttpURLConnection, URL}
import java.nio.charset.StandardCharsets

import io.apibuilder.commons.config.{Config, Profile}
import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.implicits._
import io.apibuilder.spec.v0.models.Service
import play.api.libs.json.{JsArray, JsBoolean, JsValue, Json}
import io.apibuilder.spec.v0.models.json._
import org.apache.commons.io.IOUtils

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

/**
 * Given a service, calls the Api Builder Validations resource to check if the
 * specification of the service is valid. We use this to test our transformations
 * on the services to ensure that when we rewrite a service it continues to be
 * valid.
 */
case class Validator() {

  def mustValidateDefaultProfile(service: Service): Unit = {
    mustValidateDefaultProfile(ApiBuilderService(service))
  }

  def mustValidateDefaultProfile(service: ApiBuilderService): Unit = {
    mustValidateDefaultProfile(MultiService(List(service)))
  }

  def mustValidateDefaultProfile(multiService: MultiService): Unit = {
    mustValidate(Config.mustFindDefaultProfile, multiService)
  }

  def mustValidate(profile: Profile, multiService: MultiService): Unit = {
    await(validate(profile, multiService)) match {
      case Valid(_) => ()
      case Invalid(errors) => {
        sys.error("Validation Failed:" + errors.toNonEmptyList.toList.mkString("\n -)"))
      }
    }
  }

  def validate(profile: Profile, multiService: MultiService): Future[ValidatedNec[String, Unit]] = {
    val js = Json.prettyPrint(toJson(multiService))
    val bytes = js.getBytes(StandardCharsets.UTF_8)

    val url = new URL(profile.apiUri + "/validations")
    val http = url.openConnection().asInstanceOf[HttpURLConnection]
    http.setRequestMethod("POST")
    http.setFixedLengthStreamingMode(bytes.length);
    http.setRequestProperty("Content-Type", "application/json; charset=UTF-8");
    http.setDoOutput(true)

    val os = http.getOutputStream
    try {
      os.write(bytes);
    } finally {
      os.close()
    }
    http.connect()

    val response = new String(IOUtils.toByteArray(http.getInputStream), StandardCharsets.UTF_8.name())
    Future.successful(
      parseResult(js, response)
    )
  }

  private def parseResult(requestBody: String, body: String): ValidatedNec[String, Unit] = {
    val js = Json.parse(body)
    if ((js \ "valid").asOpt[JsBoolean].exists(_.value)) {
      // println(s"ApiBuilder Validation Succeeded for body:\n$requestBody\n\n")
      ().validNec
    } else {
      println(s"ApiBuilder Validation Failed for body:\n$requestBody\n\n")
      (js \ "errors").as[JsArray].value.map(_.toString).mkString(", ").invalidNec
    }
  }

  private def await[T](f: Future[T]): T = {
    Await.result(f, FiniteDuration(10, SECONDS))
  }

  private def toJson(multiService: MultiService): JsValue = {
    multiService.services match {
      case Nil => Json.obj()
      case one :: Nil => Json.toJson(one.service)
      case _ => sys.error("Cannot serialize multiple services")
    }

  }
}
