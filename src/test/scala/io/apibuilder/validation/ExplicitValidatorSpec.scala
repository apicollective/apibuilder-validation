package io.apibuilder.validation

import io.apibuilder.validation.helpers.Helpers
import play.api.libs.json.Json
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec

class ExplicitValidatorSpec extends AnyFunSpec with Matchers with Helpers {

  private[this] lazy val validator = JsonValidator(
    loadService("apibuilder-explicit-validation-service.json")
  )

  it("properly identifies invalid object") {
    val form = Json.obj(
      "price" -> Json.obj(
        "amount" -> "a",
        "currency" -> "USD"
      )
    )

    validator.validate("object_with_price_form", form, defaultNamespace = None) match {
      case Left(errors) => errors should equal(
        Seq("object_with_price_form.price.amount must be a valid decimal")
      )
      case Right(_) => sys.error("Expected errors but got none")
    }
  }
}
