package io.apibuilder.validation

import io.apibuilder.spec.v0.models.Service
import io.apibuilder.spec.v0.models.json._
import io.flow.v0.models.json._
import io.flow.v0.models.{CardForm, HarmonizedItemForm, ItemForm}
import org.joda.time.DateTime
import org.scalatest.{FunSpec, Matchers}
import play.api.libs.json._

class JsonValidatorUnionSpec extends FunSpec with Matchers {

  private[this] lazy val service = {
    val contents = scala.io.Source.fromFile("src/test/resources/apibuilder-explicit-validation-service.json", "UTF-8").getLines.mkString("\n")
    Json.parse(contents).as[Service]
  }

  lazy val validator = JsonValidator(service)

  it("understands default discriminator") {
    val js = Json.obj(
      "id" -> "123",
      "quantity" -> 1
    )

    validator.validate(
      "cart_add_form",
      js
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
      )
    ) should equal(
      Left(Seq(
        "Invalid discriminator 'bad' for union type 'cart_add_form': must be one of 'single_cart_add_form', 'multi_cart_add_form'"
      ))
    )
  }

}
