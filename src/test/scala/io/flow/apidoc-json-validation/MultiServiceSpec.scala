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
}
