package io.flow.lib.apidoc.json.validation

import play.api.libs.json._
import org.scalatest.{FunSpec, Matchers}

class MultiServiceSpec extends FunSpec with Matchers {

  private[this] lazy val multi = {
    val base = "file://" + new java.io.File(".").getAbsolutePath
    MultiService.fromUrls(
      Seq(
        s"$base/src/test/resources/flow-api-service.json",
        s"$base/src/test/resources/apidoc-api-service.json"
      )
    )match {
      case Left(errors) => sys.error(s"Failed to load: $errors")
      case Right(s) => s
    }
  }

  it("loads multiple services") {
    multi.services.map(_.service.name) should equal(Seq("API", "apidoc api"))
  }

  it("bodyTypeFromPath") {
    multi.bodyTypeFromPath("POST", "/foo") should equal(None)

    // resources from flow api
    multi.bodyTypeFromPath("POST", "/users") should equal(Some("user_form"))
    multi.bodyTypeFromPath("POST", "/:organization/webhooks") should equal(Some("webhook_form"))

    // resources from apidoc api
    multi.bodyTypeFromPath("POST", "/:orgKey") should equal(Some("application_form"))
  }

  it("parametersFromPath") {
    multi.parametersFromPath("POST", "/foo") should be(None)
    multi.parametersFromPath("POST", "/users") should be(Some(Nil))
    multi.parametersFromPath("GET", "/users").get.map(_.name) should be(Seq("id", "email", "status", "limit", "offset", "sort"))
  }
  
  it("validate") {
    // path from flow api
    multi.upcast(
      "POST",
      "/:organization/webhooks",
      Json.obj("url" -> "https://test.flow.io")
    ) should equal(
      Left(Seq("Missing required field for type 'webhook_form': 'events'"))
    )

    // path from apidoc api
    multi.upcast(
      "POST",
      "/:orgKey",
      Json.obj("url" -> "https://test.flow.io")
    ) should equal(
      Left(Seq("Missing required fields for type 'application_form': 'name', 'visibility'"))
    )
  }

  it("url query example") {
    val form = FormData.parseEncoded("name=John%20Doe&expiration_month=12&expiration_year=2017&cvv=123&cipher=VnHzBw%2BbaGrKZL0fimklhKupHJeowxK2Mqa9LbECCnb3R%2FxIgS1vr0sFg2mUGsXR7bsNV61UURB91VrWr19V1g%3D%3D&challenge%5Btext%5D=Flow&challenge%5Bcipher%5D=df2BQZykhnTfIVIX6Vg9yjUmyEprz3dLmUYU0O8GeyCZ0t3pn1nXSP7DRDfsZAASwtNupqyYx3G4W%2BmGlWQreg%3D%3D&callback=__flowjsonp0&method=post")

    val js = multi.upcast(
      "POST",
      "/:organization/cards",
      Json.toJson(form)
    ).right.get

    (js \ "name").as[JsString].value should equal("John%20Doe")
    (js \ "expiration_month").as[JsNumber].value should equal(12)
    (js \ "expiration_year").as[JsNumber].value should equal(2017)
    (js \ "cvv").as[JsString].value should equal("123")
  }

  it("simple number example") {
    val form = Json.obj(
      "url" -> 123,
      "events" -> 456
    )

    val js = multi.upcast(
      "POST",
      "/:organization/webhooks",
      Json.toJson(form)
    ).right.get

    (js \ "url").as[JsString].value should equal("123")
    (js \ "events").as[JsArray] should equal(JsArray(Seq(JsString("456"))))
  }
}
