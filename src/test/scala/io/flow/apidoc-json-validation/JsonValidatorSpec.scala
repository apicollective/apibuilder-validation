package io.flow.lib.apidoc.json.validation

import io.flow.v0.models.{Address, CardForm, EventType, WebhookForm}
import io.flow.v0.models.json._
import com.bryzek.apidoc.spec.v0.models.Service
import com.bryzek.apidoc.spec.v0.models.json._
import play.api.libs.json._
import org.scalatest.{FunSpec, Matchers}

class JsonValidatorSpec extends FunSpec with Matchers {

  lazy val service = {
    val contents = scala.io.Source.fromFile("src/test/resources/flow-api-service.json", "UTF-8").getLines.mkString("\n")
    Json.parse(contents).as[Service]
  }

  lazy val validator = JsonValidator(service)

  it("1 required field") {
    validator.validate(
      "webhook_form",
      Json.obj("url" -> "https://test.flow.io")
    ) should equal(
      Left(Seq("Missing required field for type 'webhook_form': 'events'"))
    )
  }

  it("multiple required fields") {
    validator.validate(
      "webhook_form",
      Json.obj()
    ) should equal(
      Left(Seq("Missing required fields for type 'webhook_form': 'url', 'events'"))
    )
  }

  it("invalid type") {
    val form = Json.obj(
      "url" -> Seq("https://a.flow.io", "https://b.flow.io"),
      "events" -> "*"
    )
    validator.validate("webhook_form", form) should equal(
      Left(
        Seq(
          "Type 'webhook_form' field 'url' must be a string and not an array",
          "Type 'webhook_form' field 'events' of type '[event_type]': must be an array and not a string"
        )
      )
    )
  }

  it("converts 'number' into a string where possible") {
    val form = Json.obj(
      "url" -> 123,
      "events" -> Seq("*")
    )
    validator.validate("webhook_form", form) should equal(
      Right(
        Json.obj(
          "url" -> "123",
          "events" -> Seq("*")
        )
      )
    )
  }

  it("converts types") {
    val form = Json.obj(
      "number" -> 123,
      "expiration_month" -> "01",
      "expiration_year" -> "2019",
      "name" -> "Joe Smith"
    )

    form.validate[CardForm] match {
      case s: JsSuccess[CardForm] => sys.error("Expected form to NOT validate")
      case e: JsError => //
    }

    val converted: JsValue = validator.validate("card_form", form).right.get

    converted.validate[CardForm] match {
      case s: JsSuccess[CardForm] => {
        val form = s.get
        form.number should be(Some("123"))
        form.expirationMonth should be(1)
        form.expirationYear should be(2019)
        form.name should be("Joe Smith")
      }
      case e: JsError => {
        sys.error(s"Expected validation to succeed but got: $e")
      }
    }
  }

  it("converted nested values in arrays") {
    val form = Json.obj(
      "url" -> JsString("https://test.flow.io"),
      "events" -> JsArray(Seq(JsString("catalog_upserted"), JsNumber(123)))
    )

    form.validate[CardForm] match {
      case s: JsSuccess[CardForm] => sys.error("Expected form to NOT validate")
      case e: JsError => //
    }

    val converted: JsValue = validator.validate("webhook_form", form).right.get

    converted.validate[WebhookForm] match {
      case s: JsSuccess[WebhookForm] => {
        val form = s.get
        form.url should be("https://test.flow.io")
        form.events should be(Seq(EventType.CatalogUpserted, EventType.UNDEFINED("123")))
      }
      case e: JsError => {
        sys.error(s"Expected validation to succeed but got: $e")
      }
    }
  }

  it("validates array values") {
    val form = Json.obj(
      "url" -> JsString("https://test.flow.io"),
      "events" -> JsArray(Seq(JsString("catalog_upserted"), Json.obj()))
    )

    form.validate[CardForm] match {
      case s: JsSuccess[CardForm] => sys.error("Expected form to NOT validate")
      case e: JsError => //
    }

    validator.validate("webhook_form", form).left.get should be(
      Seq("Type 'webhook_form' field 'events' of type '[event_type]': element in position[1] must be a string and not an object")
    )
  }
  
}
