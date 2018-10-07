package io.apibuilder.validation

import io.apibuilder.validation.helpers.Helpers
import org.scalatest.{FunSpec, Matchers}
import play.api.libs.json._

class JsonValidatorUnionSpec extends FunSpec with Matchers with Helpers {

  private[this] lazy val validator = JsonValidator(
    loadService("apibuilder-explicit-validation-service.json").service
  )

  it("understands default discriminator") {
    val js = Json.obj(
      "id" -> "123",
      "quantity" -> 1
    )

    validator.validate(
      "cart_add_form",
      js,
      defaultNamespace = None
    ) should equal(
      Right(js)
    )
  }

  it("validates discriminator for union type with a default") {
    validator.validate(
      "cart_add_form",
      Json.obj(
        "discriminator" -> "bad",
        "id" -> "123",
        "quantity" -> 1
      ),
      defaultNamespace = None
    ) should equal(
      Left(Seq(
        "Invalid discriminator 'bad' for union type 'cart_add_form': must be one of 'single_cart_add_form', 'multi_cart_add_form'"
      ))
    )
  }

}
