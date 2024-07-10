package io.apibuilder.validation

import io.apibuilder.builders.ApiBuilderServiceBuilders
import io.apibuilder.helpers.TestHelpers
import io.apibuilder.validation.helpers.Helpers
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.{JsObject, Json}

class ApibuilderUnionDiscriminatorValueSpec extends AnyWordSpec with Matchers with Helpers with TestHelpers with ApiBuilderServiceBuilders {

  "uses discriminator values" in {
    val service = makeService(
      models = Seq(
        makeModel("user_form_full", fields = Seq(
          makeField("id")
        )),
        makeModel("user_form_guest", fields = Seq(
          makeField("id")
        ))
      ),
      unions = Seq(
        makeUnion("user_form", discriminator = Some("discriminator"), types = Seq(
          makeUnionType("user_form_full", discriminatorValue = Some("full")),
          makeUnionType("user_form_guest", discriminatorValue = Some("guest"))
        ))
      )
    )

    def validate(js: JsObject) = JsonValidator(ApiBuilderService(service)).validate(
      "user_form", js, defaultNamespace = None
    )

    expectValidNec {
      validate(Json.obj(
        "discriminator" -> "full",
        "id" -> "1"
      ))
    }

    expectValidNec {
      validate(Json.obj(
        "discriminator" -> "guest",
        "id" -> "1"
      ))
    }

    expectInvalidNec {
      validate(Json.obj(
        "discriminator" -> "other",
        "id" -> "1"
      ))
    } mustBe Seq("Invalid discriminator 'other' for union type 'user_form': must be one of 'full', 'guest'")
  }
}
