package io.flow.lib.apidoc.json.validation

import com.bryzek.apidoc.spec.v0.models.Service
import com.bryzek.apidoc.spec.v0.models.json._
import io.flow.v0.models.{Address, CardForm, EventType, ItemForm, WebhookForm}
import io.flow.v0.models.json._
import play.api.libs.json._
import org.scalatest.{FunSpec, Matchers}

class ApidocServiceSpec extends FunSpec with Matchers {

  lazy val service = {
    val contents = scala.io.Source.fromFile("src/test/resources/flow-api-service.json", "UTF-8").getLines.mkString("\n")
    ApidocService(
      "test",
      Json.parse(contents).as[Service]
    )
  }

  it("fromUrl") {
    ApidocService.fromUrl("file://non-existent-tmp").left.getOrElse {
      sys.error("Expected error from invalid url")
    }

    val url = "http://apidoc.me/bryzek/apidoc-common/latest/service.json"
    ApidocService.fromUrl(url).right.getOrElse {
      sys.error(s"Failed to load service from url[$url]")
    }.service.name should be("apidoc common")
  }

  it("typeFromPath") {
    service.typeFromPath("POST", "/foo") should be(None)
    service.typeFromPath("POST", "/users") should be(Some("user_form"))
    service.typeFromPath("POST", "/:organization/webhooks") should be(Some("webhook_form"))
  }

  it("validate") {
    service.validate(
      "POST",
      "/:organization/webhooks",
      Json.obj("url" -> "https://test.flow.io")
    ) should equal(
      Left(Seq("Missing required field for type 'webhook_form': 'events'"))
    )
  }
  

}
