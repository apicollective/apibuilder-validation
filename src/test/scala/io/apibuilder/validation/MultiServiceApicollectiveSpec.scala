package io.apibuilder.validation

import org.scalatest.{FunSpec, Matchers}
import play.api.libs.json.Json

class MultiServiceApicollectiveSpec extends FunSpec with Matchers {

  private[this] lazy val multi = {
    val base = "file://" + new java.io.File(".").getAbsolutePath
    MultiService.fromUrls(
      Seq(
        s"$base/src/test/resources/apibuilder-explicit-validation-core-service.json",
        s"$base/src/test/resources/apibuilder-explicit-validation-service.json"
      )
    ) match {
      case Left(errors) => sys.error(s"Failed to load: $errors")
      case Right(s) => s
    }
  }

  it("validates imported enums") {
    multi.upcast(
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
