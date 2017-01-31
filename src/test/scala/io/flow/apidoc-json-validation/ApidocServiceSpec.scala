package io.flow.lib.apidoc.json.validation

import com.bryzek.apidoc.spec.v0.models.Method
import play.api.libs.json._
import org.scalatest.{FunSpec, Matchers}

class ApidocServiceSpec extends FunSpec with Matchers {

  def readFile(name: String): String = {
    scala.io.Source.fromFile("src/test/resources/" + name, "UTF-8").getLines.mkString("\n")
  }

  private[this] lazy val service = ApidocService.toService(readFile("flow-api-service.json")).right.get

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

  it("bodyTypeFromPath") {
    service.bodyTypeFromPath("POST", "/foo") should be(None)
    service.bodyTypeFromPath("POST", "/users") should be(Some("user_form"))
    service.bodyTypeFromPath("POST", "/:organization/webhooks") should be(Some("webhook_form"))
  }

  it("parametersFromPath") {
    service.parametersFromPath("POST", "/foo") should be(None)
    service.parametersFromPath("POST", "/users") should be(Some(Nil))
    service.parametersFromPath("GET", "/users").get.map(_.name) should be(Seq("id", "email", "status", "limit", "offset", "sort"))
  }

  it("unknown path") {
    service.operationsByMethod("/other").get(Method.Options) should be(None)
  }

  it("resolves for known paths") {
    service.bodyTypeFromPath("POST", "/users") should be(Some("user_form"))
    service.bodyTypeFromPath("post", "/:organization/orders") should be(Some("order_form"))
    service.bodyTypeFromPath("PUT", "/:organization/orders/:number") should be(Some("order_put_form"))
    service.bodyTypeFromPath("DELETE", "/:organization/orders/:number") should be(None)
  }

  it("offers validation error w/ verb replacement") {
    service.upcast(
      "OPTIONS",
      "/:organization/webhooks",
      Json.obj("url" -> "https://test.flow.io", "events" -> "*")
    ) should equal(
      Left(Seq("HTTP method OPTIONS not supported for path /:organization/webhooks - Available methods: GET, POST"))
    )
  }

  it("validate") {
    service.upcast(
      "POST",
      "/:organization/webhooks",
      Json.obj("url" -> "https://test.flow.io")
    ) should equal(
      Left(Seq("Missing required field for type 'webhook_form': 'events'"))
    )
  }

}
