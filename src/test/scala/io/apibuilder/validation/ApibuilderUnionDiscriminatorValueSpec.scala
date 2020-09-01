package io.apibuilder.validation

import io.apibuilder.validation.helpers.Helpers
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec
import play.api.libs.json.Json

class ApibuilderUnionDiscriminatorValueSpec extends AnyFunSpec with Matchers with Helpers {

  private[this] lazy val validator = JsonValidator(
    loadService("apibuilder-union-discriminator-value.json")
  )

  it("uses discriminator values") {
    val userForm = Json.obj(
      "discriminator" -> "full",
      "id" -> "1"
    )
    rightOrErrors {
      validator.validate("user_form", userForm, defaultNamespace = None)
    }

    val guestForm = Json.obj(
      "discriminator" -> "guest",
      "id" -> "1"
    )
    rightOrErrors {
      validator.validate("user_form", guestForm, defaultNamespace = None)
    }

    val otherForm = Json.obj(
      "discriminator" -> "other",
      "id" -> "1"
    )
    validator.validate("user_form", otherForm, defaultNamespace = None) should be(
      Left(Seq(
        "Invalid discriminator 'other' for union type 'user_form': must be one of 'guest', 'full'"
      ))
    )
  }
}
