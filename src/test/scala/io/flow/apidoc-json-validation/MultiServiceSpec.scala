package io.flow.lib.apidoc.json.validation

import com.bryzek.apidoc.spec.v0.models.Service
import com.bryzek.apidoc.spec.v0.models.json._
import io.flow.v0.models.{Address, CardForm, EventType, ItemForm, WebhookForm}
import io.flow.v0.models.json._
import play.api.libs.json._
import org.scalatest.{FunSpec, Matchers}

class MultiServiceSpec extends FunSpec with Matchers {

  lazy val multi = MultiService.fromUrls(
    Seq(
      "http://apidoc.me/flow/api/latest/service.json",
      "http://apidoc.me/bryzek/apidoc-api/latest/service.json"
    )
  ).right.getOrElse {
    sys.error("Failed to load")
  }

  it("loads multiple services") {
    multi.services.map(_.service.name) should be(Seq("API", "apidoc api"))
  }

  it("typeFromPath") {
    multi.typeFromPath("POST", "/foo") should be(None)

    // resources from flow api
    multi.typeFromPath("POST", "/users") should be(Some("user_form"))
    multi.typeFromPath("POST", "/:organization/webhooks") should be(Some("webhook_form"))

    // resources from apidoc api
    multi.typeFromPath("POST", "/:orgKey") should be(Some("application_form"))
  }

  it("validate") {
    // path from flow api
    multi.validate(
      "POST",
      "/:organization/webhooks",
      Json.obj("url" -> "https://test.flow.io")
    ) should equal(
      Left(Seq("Missing required field for type 'webhook_form': 'events'"))
    )

    // path from apidoc api
    multi.validate(
      "POST",
      "/:orgKey",
      Json.obj("url" -> "https://test.flow.io")
    ) should equal(
      Left(Seq("Missing required fields for type 'application_form': 'name', 'visibility'"))
    )
  }

  it("url query example") {
    val form = FormData.parseEncoded("name=John%20Doe&expiration_month=12&expiration_year=2017&cvv=123&cipher=VnHzBw%2BbaGrKZL0fimklhKupHJeowxK2Mqa9LbECCnb3R%2FxIgS1vr0sFg2mUGsXR7bsNV61UURB91VrWr19V1g%3D%3D&challenge%5Btext%5D=Flow&challenge%5Bcipher%5D=df2BQZykhnTfIVIX6Vg9yjUmyEprz3dLmUYU0O8GeyCZ0t3pn1nXSP7DRDfsZAASwtNupqyYx3G4W%2BmGlWQreg%3D%3D&callback=__flowjsonp0&method=post")

    val js = multi.validate(
      "POST",
      "/:organization/cards",
      Json.toJson(form)
    ).right.get

    (js \ "name").as[JsString].value should be("John%20Doe")
    (js \ "expiration_month").as[JsNumber].value should be(12)
    (js \ "expiration_year").as[JsNumber].value should be(2017)
    (js \ "cvv").as[JsString].value should be("123")
  }
}
