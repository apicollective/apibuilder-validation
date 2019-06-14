package io.apibuilder.validation

import io.apibuilder.validation.helpers.Helpers
import org.scalatest.{FunSpec, Matchers}
import play.api.libs.json.Json

class MultiServiceApicollectiveSpec extends FunSpec with Matchers with Helpers {

  it("validates imported enums") {
    apibuilderMultiService.upcastOperationBody(
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
