package io.flow.lib.apidoc.json.validation

import play.api.libs.json._
import org.scalatest.{FunSpec, Matchers}

class MultiServiceSpec2 extends FunSpec with Matchers {

  lazy val multi = {
    val base = "file://" + new java.io.File(".").getAbsolutePath()
    MultiService.fromUrls(
      Seq(
        s"$base/src/test/resources/multi/api.service.json",
        s"$base/src/test/resources/multi/api-internal.service.json"
      )
    )match {
      case Left(errors) => sys.error(s"Failed to load: $errors")
      case Right(s) => s
    }
  }

  it("resolves body when path exists in both services") {
    multi.bodyTypeFromPath("POST", "/:organization/payments") should equal(Some("payment_form"))
  }

  /**
    * This test has two services that define methods like:
    *
    * Service1:
    *   - GET /:organization/payments
    *   - POST /:organization/payments
    *
    * Service2:
    *   - GET /:organization/payments
    *
    * We are testing that we can correctly resolve the POST, fixing a bug where
    * we were propagating the fact that service2 does not define POST through the
    * validation methods (vs. correctly resolving service 1)
    */
  it("validates when path exists in both services with different available methods") {
    multi.upcast(
      "POST",
      "/:organization/payments",
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
