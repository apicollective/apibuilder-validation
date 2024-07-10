package io.apibuilder.validation

import io.apibuilder.validation.helpers.Helpers
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec
import play.api.libs.json.Json

class MultiServiceApicollectiveSpec extends AnyFunSpec with Matchers with Helpers {

  it("validates imported enums") {
    apiBuilderMultiService.upcastOperationBody(
      "POST",
      "/people",
      Json.obj(
        "name" -> "Joe",
        "gender" -> ""
      )
    ) should equal(
      Left(
        Seq("person_form.gender invalid value ''. Valid values for the enum 'gender' are: 'male', 'female'")
      )
    )
  }

}
