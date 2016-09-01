package io.flow.lib.apidoc.json.validation

import com.bryzek.apidoc.spec.v0.models.{Contact, Service}
import com.bryzek.apidoc.spec.v0.models.json._
import play.api.libs.json._
import org.scalatest.{FunSpec, Matchers}

class SampleSpec extends FunSpec with Matchers {

  lazy val service = {
    val contents = scala.io.Source.fromFile("src/test/resources/apidoc-api-service.json", "UTF-8").getLines.mkString("\n")
    Json.parse(contents).as[Service]
  }

  lazy val validator = JsonValidator(service)

  it("1 required field") {
    val form = Json.obj()
    validator.validate("attribute_form", form) should equal(Seq("Missing required field for attribute_form: 'name'"))
  }

  it("multiple required fields") {
    val form = Json.obj()
    validator.validate("application", form) should equal(Seq("Missing required fields for application: 'guid', 'organization', 'name', 'key', 'visibility', 'audit'"))
  }

  it("invalid type") {
    val form = Json.obj(
      "name" -> Seq("Joe", "Jr")
    )
    validator.validate("attribute_form", form) should equal(Seq("Field attribute_form.name must be a string and not an array"))
  }

}
