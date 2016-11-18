package io.flow.lib.apidoc.json.validation

import com.bryzek.apidoc.spec.v0.models.Service
import com.bryzek.apidoc.spec.v0.models.json._
import io.flow.v0.models.{Address, CardForm, EventType, ItemForm, WebhookForm}
import io.flow.v0.models.json._
import play.api.libs.json._
import org.scalatest.{FunSpec, Matchers}

class ApidocServiceSpec extends FunSpec with Matchers {

  def readFile(name: String): String = {
    scala.io.Source.fromFile("src/test/resources/" + name, "UTF-8").getLines.mkString("\n")
  }

  def readUrl(url: String): String = {
    val path = url.toLowerCase.stripPrefix("http://").stripPrefix("https://").split("/").toList.mkString("/")
    println("path: " + path)
    readFile(path)
  }

  lazy val service = ApidocService.toService(readFile("flow-api-service.json")).right.get

  it("fromUrl") {
    ApidocService.fromUrl("file://non-existent-tmp").left.getOrElse {
      sys.error("Expected error from invalid url")
    }

    val result = ApidocService.toService(readFile("apidoc-common-service.json")) match {
      case Left(errors) => sys.error(s"Failed to load service: $errors")
      case Right(s) => s
    }
    result.service.name should be("apidoc common")
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
