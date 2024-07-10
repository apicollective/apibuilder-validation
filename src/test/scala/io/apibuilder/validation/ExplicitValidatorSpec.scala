package io.apibuilder.validation

import io.apibuilder.builders.ApiBuilderServiceBuilders
import io.apibuilder.helpers.TestHelpers
import io.apibuilder.validation.helpers.Helpers
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.Json

class ExplicitValidatorSpec extends AnyWordSpec with Matchers with Helpers  with TestHelpers with ApiBuilderServiceBuilders {
  {

    "properly identifies invalid object" in {
      val service = makeService(
        models = Seq(makeModel("price", fields = Seq(
          makeField("amount", `type` = "decimal"),
          makeField("currency"),
        )))
      )

      val validator = JsonValidator(ApiBuilderService(service))

      expectInvalidNec {
        validator.validate("price", Json.obj(
          "amount" -> "a",
          "currency" -> "USD"
        ), defaultNamespace = None)
      } mustBe Seq("price.amount must be a valid decimal")
    }
  }
}