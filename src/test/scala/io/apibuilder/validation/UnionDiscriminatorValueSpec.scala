package io.apibuilder.validation

import cats.data.ValidatedNec
import io.apibuilder.builders.ApiBuilderServiceBuilders
import io.apibuilder.helpers.{Helpers, TestHelpers}
import io.apibuilder.spec.v0.models.Service
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.{JsObject, JsValue, Json}

class UnionDiscriminatorValueSpec extends AnyWordSpec with Matchers with Helpers with TestHelpers with ApiBuilderServiceBuilders {

  private val baseService = makeService(
    models = Seq(
      makeModel("user_form_full", fields = Seq(
        makeField("email")
      )),
      makeModel("user_form_guest", fields = Seq(
        makeField("name")
      ))
    )
  )

  private def validate(service: Service, js: JsObject): ValidatedNec[String, JsValue] = JsonValidator(ApiBuilderService(service)).validate(
    "user_form", js, defaultNamespace = None
  )


  "uses discriminator values" in {
    val service = baseService.copy(
      unions = Seq(
        makeUnion("user_form", discriminator = Some("discriminator"), types = Seq(
          makeUnionType("user_form_full", discriminatorValue = Some("full")),
          makeUnionType("user_form_guest", discriminatorValue = Some("guest"))
        ))
      )
    )

    expectValidNec {
      validate(service, Json.obj(
        "discriminator" -> "full",
        "email" -> randomEmail()
      ))
    }

    expectValidNec {
      validate(service, Json.obj(
        "discriminator" -> "guest",
        "name" -> "foo bar"
      ))
    }

    expectInvalidNec {
      validate(service, Json.obj(
        "discriminator" -> "other",
        "id" -> "1"
      ))
    } mustBe Seq("Invalid discriminator 'other' for union type 'user_form': must be one of 'full', 'guest'")
  }


  "default discriminator" must {
    val service = baseService.copy(
      unions = Seq(
        makeUnion("user_form", discriminator = Some("discriminator"), types = Seq(
          makeUnionType("user_form_full", default = Some(true)),
          makeUnionType("user_form_guest")
        ))
      )
    )

    "uses default when discriminator is not specified" in {
      expectValidNec {
        validate(service, Json.obj("email" -> randomEmail()))
      }
    }

    "validates discriminator" in {
      expectInvalidNec {
        validate(service, Json.obj("discriminator" -> "other"))
      } mustBe Seq("Invalid discriminator 'other' for union type 'user_form': must be one of 'user_form_full', 'user_form_guest'")
    }
  }
}
