package io.apibuilder.validation

import io.apibuilder.spec.v0.models.Service
import io.apibuilder.spec.v0.models.json._
import play.api.libs.json.Json
import org.scalatest.{FunSpec, Matchers}

class ExplicitValidatorSpec extends FunSpec with Matchers {

  lazy val service = {
    val contents = scala.io.Source.fromFile("src/test/resources/apibuilder-explicit-validation-service.json", "UTF-8").getLines.mkString("\n")
    Json.parse(contents).as[Service]
  }

  lazy val validator = JsonValidator(service)

  it("properly identifies invalid object") {
    val form = Json.obj(
      "price" -> Json.obj(
        "amount" -> "a",
        "currency" -> "USD"
      )
    )

    validator.validate("object_with_price_form", form) match {
      case Left(errors) => errors should equal(
        Seq("Type 'object_with_price_form' field 'price.amount' must be a valid decimal")
      )
      case Right(_) => sys.error(s"Expected errors but got none")
    }
  }
}
