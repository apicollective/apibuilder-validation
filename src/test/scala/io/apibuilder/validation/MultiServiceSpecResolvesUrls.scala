package io.apibuilder.validation

import io.apibuilder.spec.v0.models.Method
import io.apibuilder.validation.helpers.Helpers
import org.scalatest.{FunSpec, Matchers}
import play.api.libs.json.Json

class MultiServiceSpecResolvesUrls extends FunSpec with Matchers with Helpers {

  it("validates unknown methods") {
    flowMultiService.validateOperation("FOO", "/test-org/payments") should equal(
      Left(Seq(
        "HTTP method 'FOO' is invalid. Must be one of: " + Method.all.map(_.toString).mkString(", "))
      )
    )

    flowMultiService.validateOperation("OPTIONS", "/test-org/payments") should equal(
      Left(Seq("HTTP method 'OPTIONS' not defined for path '/test-org/payments' - Available methods: GET, POST"))
    )
  }

  it("validates unknown paths") {
    flowMultiService.validateOperation("GET", "/foo") should equal(
      Left(Seq("HTTP path '/foo' is not defined"))
    )
  }

  it("validates unknown method for a known path") {
    flowMultiService.validateOperation("OPTIONS", "/users") should equal(
      Left(Seq("HTTP method 'OPTIONS' not defined for path '/users' - Available methods: GET, POST"))
    )
  }

  it("resolves body when path exists in both services") {
    flowMultiService.bodyTypeFromPath("POST", "/test-org/payments").map(_.name) should equal(Some("payment_form"))
  }

  it("resolves body when there are multiple variables in path") {
    flowMultiService.bodyTypeFromPath("POST", "/demo/shipping/configuration/copies/key").map(_.name) should equal(
      Some("shipping_configuration_copy_form")
    )
  }

  /**
    * This test has two services that define methods like:
    *
    * Service1:
    *   - GET /test-org/payments
    *   - POST /test-org/payments
    *
    * Service2:
    *   - GET /test-org/payments
    *
    * We are testing that we can correctly resolve the POST, fixing a bug where
    * we were propagating the fact that service2 does not define POST through the
    * validation methods (vs. correctly resolving service 1)
    */
  it("validates when path exists in both services with different available methods") {
    rightOrErrors {
      flowMultiService.upcastOperationBody(
        "POST",
        "/test-org/payments",
        Json.obj(
          "discriminator" -> "merchant_of_record_payment_form",
          "method" -> "paypal",
          "order_number" -> "F1001",
          "amount" -> 1.00,
          "currency" -> "CAD"
        )
      )
    }
  }

  it("resolves static methods over dynamic ones") {
    flowMultiService.asInstanceOf[MultiServiceImpl].validateOperation(Method.Post, "/demo/tokens").right.get.service.name should equal("API")
    flowMultiService.bodyTypeFromPath("POST", "/:organization/tokens").map(_.name) should equal(
      Some("organization_token_form")
    )

    flowMultiService.asInstanceOf[MultiServiceImpl].validateOperation(Method.Post, "/users/tokens").right.get.service.name should equal("API Internal")
    flowMultiService.bodyTypeFromPath("POST", "/users/tokens") should equal(
      None
    )
  }

}
