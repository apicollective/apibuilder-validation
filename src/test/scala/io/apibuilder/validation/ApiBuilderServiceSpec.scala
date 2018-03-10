package io.apibuilder.validation

import io.apibuilder.spec.v0.models.Method
import io.apibuilder.validation.helpers.Helpers
import org.scalatest.{FunSpec, Matchers}

class ApiBuilderServiceSpec extends FunSpec with Matchers with Helpers {

  private[this] lazy val service = loadService("flow-api-service.json")

  it("fromUrl") {
    ApiBuilderService.fromUrl("file://non-existent-tmp").left.getOrElse {
      sys.error("Expected error from invalid url")
    }

    rightOrErrors {
      ApiBuilderService.toService(readFile("apibuilder-common-service.json"))
    }.service.name should be("apibuilder common")
  }

  it("bodyTypeFromPath") {
    service.bodyTypeFromPath("POST", "/foo") should be(None)
    service.bodyTypeFromPath("POST", "/users") should be(Some("user_form"))
    service.bodyTypeFromPath("POST", "/:organization/webhooks") should be(Some("webhook_form"))
  }

  it("operation") {
    service.operation("POST", "/foo") should be(None)

    val op = service.operation("POST", "/users").get
    op.method should equal(Method.Post)
    op.path should equal("/users")
    op.parameters should be(Nil)

    service.operation("GET", "/users").get.parameters.map(_.name) should be(
      Seq("id", "email", "status", "limit", "offset", "sort")
    )
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
