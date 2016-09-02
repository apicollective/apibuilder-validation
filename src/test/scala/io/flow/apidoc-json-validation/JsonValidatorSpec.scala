package io.flow.lib.apidoc.json.validation

import com.bryzek.apidoc.api.v0.models.AttributeForm
import com.bryzek.apidoc.api.v0.models.json._
import com.bryzek.apidoc.spec.v0.models.Service
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
    validator.validate("attribute_form", form) should equal(
      Left(Seq("Missing required field for attribute_form: 'name'"))
    )
  }

  it("multiple required fields") {
    val form = Json.obj()
    validator.validate("application", form) should equal(
      Left(Seq("Missing required fields for application: 'guid', 'organization', 'name', 'key', 'visibility', 'audit'"))
    )
  }

  it("invalid type") {
    val form = Json.obj(
      "name" -> Seq("Joe", "Jr")
    )
    validator.validate("attribute_form", form) should equal(
      Left(Seq("Field attribute_form.name must be a string and not an array"))
    )
  }

  it("converts 'number' into a string where possible") {
    val form = Json.obj(
      "name" -> 123
    )
    validator.validate("attribute_form", form) should equal(
      Right(
        Json.obj(
          "name" -> "123"
        )
      )
    )
  }

  it("Json validation") {
    val form = Json.obj(
      "name" -> 123
    )

    form.validate[AttributeForm] match {
      case s: JsSuccess[AttributeForm] => sys.error("Expected form to NOT validate")
      case e: JsError => //
    }

    val converted: JsValue = validator.validate("attribute_form", form).right.get
    
    converted.validate[AttributeForm] match {
      case s: JsSuccess[AttributeForm] => {
        val form = s.get
        form.name should be("123")
      }
      case e: JsError => sys.error("Expected validation to succeed")
    }
  }

}
