package io.flow.lib.apidoc.json.validation

import com.bryzek.apidoc.spec.v0.models.Service
import com.bryzek.apidoc.spec.v0.models.json._
import io.flow.v0.models.{Address, CardForm, EventType, HarmonizedItemForm, ItemForm, WebhookForm}
import io.flow.v0.models.json._
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
      "events" -> Seq("*")
    )
    validator.validate("webhook_form", form) should equal(
      Left(
        Seq(
          "Type 'webhook_form' field 'url' must be a string and not an array"
        )
      )
    )
  }

  it("Upcasts singletons to arrays") {
    val form = Json.obj(
      "url" -> "https://a.flow.io",
      "events" -> "*"
    )
    validator.validate("webhook_form", form) should equal(
      Right(
        Json.obj(
          "url" -> "https://a.flow.io",
          "events" -> Seq("*")
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

  it("converts booleans where possible") {
    (
      Booleans.TrueValues.map(JsString(_)) ++ Seq(JsNumber(1))
    ).foreach { v =>
      val form = Json.obj(
        "code" -> "match",
        "name" -> v
      )
      validator.validate("avs", form) should equal(
        Right(
          Json.obj(
            "code" -> "match",
            "name" -> true
          )
        )
      )
    }

    (
      Booleans.FalseValues.map(JsString(_)) ++ Seq(JsNumber(0))
    ).foreach { v =>
      val form = Json.obj(
        "code" -> "match",
        "name" -> v
      )
      validator.validate("avs", form) should equal(
        Right(
          Json.obj(
            "code" -> "match",
            "name" -> false
          )
        )
      )
    }
  }

  it("validates enum values") {
    val form = Json.obj(
      "code" -> "bad"
    )
    validator.validate("avs", form) should equal(
      Left(
        Seq("Type 'avs' field 'code' invalid value 'bad'. Valid values for the enum 'avs_code' are: 'match', 'partial', 'unsupported', 'no_match'")
      )
    )
  }
  
  it("validates nested models") {
    val form = Json.obj(
      "number" -> 123,
      "expiration_month" -> "01",
      "expiration_year" -> "2019",
      "name" -> "Joe Smith",
      "address" -> Json.obj(
        "streets" -> JsArray(Seq(JsString("1 main st"), JsNull))
      )
    )

    validator.validate("card_form", form) match {
      case Left(errors) => errors should equal(
        Seq("Type 'card_form' field 'address' of type '[string]': element in position[1] must be a string and not null")
      )
      case Right(js) => sys.error("Expected form to NOT validate")
    }
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

    val converted: JsValue = validator.validate("card_form", form) match {
      case Left(errors) => sys.error(errors.mkString(", "))
      case Right(js) => js
    }

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
      "name" -> JsString("Test item"),
      "number" -> JsString("sku-1"),
      "categories" -> JsArray(Seq(JsString("a"), JsNumber(123)))
    )

    form.validate[HarmonizedItemForm] match {
      case s: JsSuccess[HarmonizedItemForm] => sys.error("Expected form to NOT validate")
      case e: JsError => //
    }

    val converted: JsValue = validator.validate("harmonized_item_form", form).right.get

    converted.validate[HarmonizedItemForm] match {
      case s: JsSuccess[HarmonizedItemForm] => {
        val form = s.get
        form.name should be("Test item")
        form.number should be("sku-1")
        form.categories should be(Some(Seq("a", "123")))
      }
      case e: JsError => {
        sys.error(s"Expected validation to succeed but got: $e")
      }
    }
  }

  it("validates array values") {
    val form = Json.obj(
      "name" -> JsString("Test item"),
      "number" -> JsString("sku-1"),
      "categories" -> JsArray(
        Seq(
          JsString("furniture"),
          Json.obj(),
          JsNull
        )
      )
    )

    form.validate[HarmonizedItemForm] match {
      case s: JsSuccess[HarmonizedItemForm] => sys.error("Expected form to NOT validate")
      case e: JsError => //
    }

    validator.validate("harmonized_item_form", form).left.get should be(
      Seq(
        "Type 'harmonized_item_form' field 'categories' of type '[string]': element in position[1] must be a string and not an object",
        "Type 'harmonized_item_form' field 'categories' of type '[string]': element in position[2] must be a string and not null"
      )
    )
  }

  it("validates maps") {
    val form = Json.obj(
      "number" -> "sku-1",
      "name" -> "test",
      "currency" -> "USD",
      "price" -> 10,
      "locale" -> "en_us",
      "attributes" -> Json.obj(
        "a" -> 1,
        "b" -> true,
        "c" -> "baz"
      )
    )

    val js = validator.validate("item_form", form) match {
      case Left(errors) => sys.error(errors.mkString(", "))
      case Right(js) => js
    }

    js.validate[ItemForm] match {
      case s: JsSuccess[ItemForm] => {
        val i = s.get
        i.attributes should be(
          Some(
            Map(
              "a" -> "1",
              "b" -> "true",
              "c" -> "baz"
            )
          )
        )
      }
      case e: JsError => {
        sys.error(s"Expected validation to succeed but got: $e")
      }
    }
  }
  
  it("returns appropriate error messages from maps") {
    val form = Json.obj(
      "number" -> "sku-1",
      "name" -> "test",
      "currency" -> "USD",
      "price" -> 10,
      "locale" -> "en_us",
      "attributes" -> Json.obj(
        "a" -> Json.obj()
      )
    )

    validator.validate("item_form", form) should be(
      Left(Seq("Type 'item_form' field 'attributes' of type 'map[string]': element[a] must be a string and not an object"))
    )
  }

  it("Properly validates null in place of array") {
    val form = Json.obj(
      "number" -> 123,
      "expiration_month" -> "01",
      "expiration_year" -> "2019",
      "name" -> "Joe Smith",
      "address" -> Json.obj(
        "streets" -> JsNull
      )
    )

    validator.validate("card_form", form) match {
      case Left(errors) => errors should equal(
        Seq("Type 'card_form' field 'address' of type '[string]': must be an array and not null")
      )
      case Right(js) => sys.error("Expected form to NOT validate")
    }
  }

  it("Properly reports errors on js objects") {
    val form = Json.obj(
      "name" -> "",
      "email" -> "rob@flow.io",
      "organization" -> "demo",
      "role" -> "member"
    )

    validator.validate("invitation_form", form) match {
      case Left(errors) => errors should equal(
        Seq("Type 'invitation_form' field 'name' must be an object and not a string")
      )
      case Right(js) => sys.error("Expected form to NOT validate")
    }
  }

}
