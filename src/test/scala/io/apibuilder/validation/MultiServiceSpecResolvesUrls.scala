package io.apibuilder.validation

import io.apibuilder.spec.v0.models.Method
import org.scalatest.{FunSpec, Matchers}
import play.api.libs.json._

class MultiServiceSpecResolvesUrls extends FunSpec with Matchers {

  private[this] lazy val multi = {
    val base = "file://" + new java.io.File(".").getAbsolutePath
    MultiService.fromUrls(
      Seq(
        s"$base/src/test/resources/flow-api-service.json",
        s"$base/src/test/resources/flow-api-internal-service.json"
      )
    ) match {
      case Left(errors) => sys.error(s"Failed to load: $errors")
      case Right(s) => s
    }
  }

  it("validates unknown operations") {
    multi.validate("FOO", "/test-org/payments") should equal(
      Left(Seq("HTTP method 'FOO' is invalid. Must be one of: " + Method.all.map(_.toString).mkString(", ")))
    )

    multi.validate("OPTIONS", "/test-org/payments") should equal(
      Left(Seq("HTTP method 'OPTIONS' not supported for path /test-org/payments - Available methods: GET, POST"))
    )
  }

  it("validates unknown paths") {
    multi.validate("GET", "/foo") should equal(
      Left(Seq("HTTP path '/foo' is not defined"))
    )
  }

  it("resolves body when path exists in both services") {
    multi.bodyTypeFromPath("POST", "/test-org/payments") should equal(Some("payment_form"))
  }

  it("resolves body when there are multiple variables in path") {
    multi.bodyTypeFromPath("POST", "/test-org/shopify/orders/123/authorizations") should equal(
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
    multi.upcast(
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
}
