package io.apibuilder.validation

import play.api.libs.json._
import org.scalatest.{FunSpec, Matchers}

class ApiBuilderServiceSpec extends FunSpec with Matchers {

  def readFile(name: String): String = {
    scala.io.Source.fromFile("src/test/resources/" + name, "UTF-8").getLines.mkString("\n")
  }

  private[this] lazy val service = ApiBuilderService.toService(readFile("flow-api-service.json")).right.get

  it("fromUrl") {
    ApiBuilderService.fromUrl("file://non-existent-tmp").left.getOrElse {
      sys.error("Expected error from invalid url")
    }

    val result = ApiBuilderService.toService(readFile("apibuilder-common-service.json")) match {
      case Left(errors) => sys.error(s"Failed to load service: $errors")
      case Right(s) => s
    }
    result.service.name should be("apibuilder common")
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

  it("resolves for known paths") {
    service.bodyTypeFromPath("POST", "/users") should be(Some("user_form"))
    service.bodyTypeFromPath("post", "/:organization/orders") should be(Some("order_form"))
    service.bodyTypeFromPath("PUT", "/:organization/orders/:number") should be(Some("order_put_form"))
    service.bodyTypeFromPath("POST", "/:organization/authorizations") should be(Some("authorization_form"))
    service.bodyTypeFromPath("DELETE", "/:organization/orders/:number") should be(None)
    service.bodyTypeFromPath("PUT", "/sessions/:session") should be(Some("session_put_form"))
  }

  it("resolves for known paths w/ variable substitution") {
    service.bodyTypeFromPath("post", "/:org/orders") should be(Some("order_form"))
    service.bodyTypeFromPath("PUT", "/sessions/:id") should be(Some("session_put_form"))
  }

}
