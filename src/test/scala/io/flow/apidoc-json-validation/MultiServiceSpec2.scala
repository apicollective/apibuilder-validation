package io.flow.lib.apidoc.json.validation

import com.bryzek.apidoc.spec.v0.models.Service
import com.bryzek.apidoc.spec.v0.models.json._
import io.flow.v0.models.{Address, CardForm, EventType, ItemForm, WebhookForm}
import io.flow.v0.models.json._
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
  
  it("validated when path exists in both services") {
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
    ) should equal(
      Left(Seq("Missing required field for type 'webhook_form': 'events'"))
    )
  }
}
