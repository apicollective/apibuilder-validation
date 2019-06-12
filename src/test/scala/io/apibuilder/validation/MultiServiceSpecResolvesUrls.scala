package io.apibuilder.validation

import io.apibuilder.spec.v0.models.Method
import io.apibuilder.validation.helpers.Helpers
import org.scalatest.{FunSpec, Matchers}
import play.api.libs.json.Json

class MultiServiceSpecResolvesUrls extends FunSpec with Matchers with Helpers {

  it("validates unknown methods") {
    flowMultiService.validate("FOO", "/test-org/payments") should equal(
      Left(Seq("HTTP method 'FOO' is invalid. Must be one of: " + Method.all.map(_.toString).mkString(", ")))
    )

    flowMultiService.validate("OPTIONS", "/test-org/payments") should equal(
      Left(Seq("HTTP method 'OPTIONS' not supported for path /test-org/payments - Available methods: GET, POST"))
    )
  }

  it("validates unknown paths") {
    flowMultiService.validate("GET", "/foo") should equal(
      Left(Seq("HTTP path '/foo' is not defined"))
    )
  }

  it("validates unknown method for a known path") {
    flowMultiService.validate("OPTIONS", "/users") should equal(
      Left(Seq("HTTP method 'OPTIONS' not supported for path /users - Available methods: GET, POST"))
    )
  }

  it("resolves body when path exists in both services") {
    flowMultiService.bodyTypeFromPath("POST", "/test-org/payments") should equal(Some("payment_form"))
  }

  it("resolves body when there are multiple variables in path") {
    flowMultiService.bodyTypeFromPath("POST", "/test-org/shopify/orders/123/authorizations") should equal(
      Some("io.flow.payment.v0.unions.authorization_form")
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
    flowMultiService.upcast(
      "POST",
      "/test-org/payments",
      Json.obj(
        "discriminator" -> "merchant_of_record_payment_form",
        "method" -> "paypal",
        "order_number" -> "F1001",
        "amount" -> 1.00,
        "currency" -> "CAD"
      )
    ).right.getOrElse {
      sys.error("Failed to validate payment_form")
    }
  }

  it("resolves static methods over dynamic ones") {
    flowMultiService.asInstanceOf[MultiServiceImpl].resolveService("POST", "/demo/tokens").right.get.service.name should equal("API")
    flowMultiService.bodyTypeFromPath("POST", "/:organization/tokens") should equal(
      Some("organization_token_form")
    )

    flowMultiService.asInstanceOf[MultiServiceImpl].resolveService("POST", "/users/tokens").right.get.service.name should equal("API Internal")
    flowMultiService.bodyTypeFromPath("POST", "/users/tokens") should equal(
      None
    )
  }

}
