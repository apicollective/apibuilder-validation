package io.apibuilder.validation

import io.apibuilder.validation.helpers.Helpers
import org.scalatest.{FunSpec, Matchers}
import play.api.libs.json.Json

class ApibuilderUnionDiscriminatorValueSpec extends FunSpec with Matchers with Helpers {

  private[this] lazy val validator = JsonValidator(
    loadService("apibuilder-union-discriminator-value.json").service
  )

  it("uses discriminator values") {
    val userForm = Json.obj(
      "discriminator" -> "user",
      "id" -> "1"
    )
    rightOrErrors {
      validator.validate("user_form", userForm)
    }

    val guestForm = Json.obj(
      "discriminator" -> "guest",
      "id" -> "1"
    )
    rightOrErrors {
      validator.validate("user_form", guestForm)
    }

    val otherForm = Json.obj(
      "discriminator" -> "other",
      "id" -> "1"
    )
    validator.validate("user_form", otherForm) should be(
      Left(Seq(
        "Invalid discriminator 'user' for union type 'user_form': must be one of 'guest', 'full'"
      ))
    )
  }
}
