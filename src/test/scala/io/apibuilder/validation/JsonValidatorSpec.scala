package io.apibuilder.validation

import io.apibuilder.validation.helpers.Helpers
import io.flow.v0.models.{CardForm, HarmonizedItemForm, ItemForm}
import io.flow.v0.models.json._
import play.api.libs.json._
import org.joda.time.DateTime
import org.scalatest.{FunSpec, Matchers}

class JsonValidatorSpec extends FunSpec with Matchers with Helpers {

  private[this] lazy val validator: JsonValidator = JsonValidator(
    loadService("flow-api-service.json").service
  )

  private[this] def validate(typ: String, js: JsValue) = {
    validator.validate(
      typ, js, defaultNamespace = None
    )
  }

  it("findType is case insensitive") {
    val m1 = validator.findType("webhook", defaultNamespace = None).head
    m1.name should equal("webhook")

    val m2 = validator.findType("WEBHOOK", defaultNamespace = None).head
    m2.name should equal("webhook")
  }

  it("1 required field") {
    validate(
      "webhook_form",
      Json.obj("url" -> "https://test.flow.io")
    ) should equal(
      Left(Seq("Missing required field for webhook_form: events"))
    )
  }

  it("multiple required fields") {
    validate(
      "webhook_form",
      Json.obj()
    ) should equal(
      Left(Seq("Missing required fields for webhook_form: url, events"))
    )
  }

  it("invalid type") {
    val form = Json.obj(
      "url" -> Seq("https://a.flow.io", "https://b.flow.io"),
      "events" -> Seq("*")
    )
    validate("webhook_form", form) should equal(
      Left(
        Seq(
          "webhook_form.url must be a string and not an array"
        )
      )
    )
  }

  it("Upcasts singletons to arrays") {
    val form = Json.obj(
      "url" -> "https://a.flow.io",
      "events" -> "*"
    )
    validate("webhook_form", form) should equal(
      Right(
        Json.obj(
          "url" -> "https://a.flow.io",
          "events" -> Seq("*")
        )
      )
    )
  }

  it("converts number into a string where possible when number is an int") {
    val form = Json.obj(
      "url" -> 123,
      "events" -> Seq("*")
    )
    validate("webhook_form", form) should equal(
      Right(
        Json.obj(
          "url" -> "123",
          "events" -> Seq("*")
        )
      )
    )
  }

  it("converts number into a string where possible when number is a double") {
    val form = Json.obj(
      "url" -> 123.45,
      "events" -> Seq("*")
    )
    validate("webhook_form", form) should equal(
      Right(
        Json.obj(
          "url" -> "123.45",
          "events" -> Seq("*")
        )
      )
    )
  }

  it("validates a double") {
    validate("double", Json.parse("123.45")).rightValue.as[Double] should equal(123.45)
    validate("double", Json.parse("123")).rightValue.as[Double] should equal(123)
    validate("double", JsString("NaN")) should equal(Left(List("double must be a valid double")))
    validate("double", JsString(" ")) should equal(Left(List("double must be a valid double")))
  }

  it("validates a decimal") {
    validate("decimal", Json.parse("123.45")).rightValue.as[BigDecimal] should equal(123.45)
    validate("decimal", Json.parse("123")).rightValue.as[BigDecimal] should equal(123)
    validate("decimal", JsString(" ")) should equal(Left(List("decimal must be a valid decimal")))
  }

  it("validates a UUID") {
    val uuid = java.util.UUID.randomUUID
    validate("uuid", JsString(uuid.toString)).rightValue.as[java.util.UUID] should equal(uuid)
    validate("uuid", JsString(" ")) should equal(Left(List("uuid must be a valid UUID")))
  }

  it("validates an ISO 8601 date (yyyy-MM-dd)") {
    validate("date-iso8601", JsString("2017-01-01")).rightValue.as[String] should equal("2017-01-01")
    validate("date-iso8601", JsString("2017-1-01")).rightValue.as[String] should equal("2017-1-01")
    validate("date-iso8601", JsString("2017-01-1")).rightValue.as[String] should equal("2017-01-1")
    validate("date-iso8601", JsString("2017-1-1")).rightValue.as[String] should equal("2017-1-1")
    validate("date-iso8601", JsString("invalid")) should equal(Left(List("date-iso8601 must be a valid ISO 8601 date. Example: '2017-07-24'")))
    // Tests that the format must be yyyy-MM-dd
    validate("date-iso8601", JsString(new DateTime(2017, 2, 24, 0, 0, 0).toString)) should equal(Left(List("date-iso8601 must be a valid ISO 8601 date. Example: '2017-07-24'")))
  }

  it("validates an ISO 8601 datetime") {
    val dt = new DateTime(2017, 1, 1, 0, 0, 0).toString
    validate("date-time-iso8601", JsString("2014-06-20T11:41:08+02:00")).rightValue.as[String] should equal("2014-06-20T11:41:08+02:00")
    validate("date-time-iso8601", JsString("2017-01-01")).rightValue.as[String] should equal("2017-01-01")
    validate("date-time-iso8601", JsString("2017-1-01")).rightValue.as[String] should equal("2017-1-01")
    validate("date-time-iso8601", JsString("2017-01-1")).rightValue.as[String] should equal("2017-01-1")
    validate("date-time-iso8601", JsString("2017-1-1")).rightValue.as[String] should equal("2017-1-1")
    validate("date-time-iso8601", JsString(dt)).rightValue.as[String] should equal(dt)
    validate("date-time-iso8601", JsString("invalid")) should equal(Left(List("date-time-iso8601 must be a valid ISO 8601 datetime. Example: '2017-07-24T09:41:08+02:00'")))
  }

  it("converts booleans where possible") {
    (
      Booleans.TrueValues.map(JsString) ++ Seq(JsNumber(1))
    ).foreach { v =>
      val form = Json.obj(
        "code" -> "match",
        "name" -> v
      )
      validate("avs", form) should equal(
        Right(
          Json.obj(
            "code" -> "match",
            "name" -> true
          )
        )
      )
    }

    (
      Booleans.FalseValues.map(JsString) ++ Seq(JsNumber(0))
    ).foreach { v =>
      val form = Json.obj(
        "code" -> "match",
        "name" -> v
      )
      validate("avs", form) should equal(
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
    validate("avs", form) should equal(
      Left(
        Seq("avs.code invalid value 'bad'. Valid values for the enum 'avs_code' are: 'match', 'partial', 'unsupported', 'no_match'")
      )
    )
  }

  it("validates enum values that are passed in as empty strings") {
    val form = Json.obj(
      "code" -> ""
    )
    validate("avs", form) should equal(
      Left(
        Seq("avs.code invalid value ''. Valid values for the enum 'avs_code' are: 'match', 'partial', 'unsupported', 'no_match'")
      )
    )
  }

  it("validates nested models") {
    val form = Json.obj(
      "number" -> 123,
      "cvv" -> 456,
      "expiration_month" -> "01",
      "expiration_year" -> "2019",
      "name" -> "Joe Smith",
      "address" -> Json.obj(
        "streets" -> JsArray(Seq(JsString("1 main st"), JsNull))
      )
    )

    validate("card_form", form) match {
      case Left(errors) => errors should equal(
        Seq("card_form.address.streets of type '[string]': element in position[1] must be a string and not null")
      )
      case Right(_) => sys.error("Expected form to NOT validate")
    }
  }


  it("converts types") {
    val form = Json.obj(
      "number" -> 123,
      "cvv" -> Seq("456"),
      "expiration_month" -> "01",
      "expiration_year" -> "2019",
      "name" -> "Joe Smith"
    )

    form.validate[CardForm] match {
      case _: JsSuccess[CardForm] => sys.error("Expected form to NOT validate")
      case _: JsError => //
    }

    val converted: JsValue = validate("card_form", form) match {
      case Left(errors) => sys.error(errors.mkString(", "))
      case Right(js) => js
    }

    converted.validate[CardForm] match {
      case s: JsSuccess[CardForm] => {
        val form = s.get
        form.number should be(Some("123"))
        form.cvv should be(Some("456"))
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
      "categories" -> JsArray(Seq(JsString("a"), JsNumber(123))),
      "currency" -> "CAD",
      "price" -> 123,
    )

    form.validate[HarmonizedItemForm] match {
      case _: JsSuccess[HarmonizedItemForm] => sys.error("Expected form to NOT validate")
      case _: JsError => //
    }

    val converted: JsValue = rightOrErrors {
      validate("harmonized_item_form", form)
    }

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
      ),
      "currency" -> "CAD",
      "price" -> 123,
    )

    form.validate[HarmonizedItemForm] match {
      case _: JsSuccess[HarmonizedItemForm] => sys.error("Expected form to NOT validate")
      case _: JsError => //
    }

    validate("harmonized_item_form", form).leftValue should be(
      Seq(
        "harmonized_item_form.categories of type '[string]': element in position[1] must be a string and not an object",
        "harmonized_item_form.categories of type '[string]': element in position[2] must be a string and not null"
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

    val js = validate("item_form", form) match {
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

    validate("item_form", form) should be(
      Left(
        Seq("item_form.attributes of type 'map[string]': element[a] must be a string and not an object")
      )
    )
  }

  it("Properly reports errors on js objects") {
    val form = Json.obj(
      "name" -> "",
      "email" -> "test-user@test.flow.io",
      "organization" -> "demo",
      "role" -> "member"
    )

    validate("invitation_form", form) match {
      case Left(errors) => errors should equal(
        Seq("invitation_form.name must be an object and not a string")
      )
      case Right(_) => sys.error("Expected form to NOT validate")
    }
  }

  it("converts array of length 1 into a single value") {
    // This supports a use case from python libraries that send single values w/ array syntax (e.g. number[]=1)
    val form = Json.obj(
      "order_number" -> Seq[String]("123"),
      "token" -> "blah",
      "discriminator" -> "merchant_of_record_authorization_form"
    )

    validate("merchant_of_record_authorization_form", form) match {
      case Left(errors) => sys.error(s"Error validating form: $errors")
      case Right(js) => {
        js should equal(
          Json.obj(
            "order_number" -> "123",
            "token" -> "blah",
            "discriminator" -> "merchant_of_record_authorization_form"
          )
        )
      }
    }
  }

  it("reports errors when expecting a single value and presented with an array of length > 1") {
    val form = Json.obj(
      "order_number" -> Seq[String]("123", "456"),
      "token" -> "blah",
      "discriminator" -> "merchant_of_record_authorization_form"
    )

    validate("merchant_of_record_authorization_form", form) match {
      case Left(errors) => errors should equal(
        Seq("merchant_of_record_authorization_form.order_number must be a string and not an array")
      )
      case Right(_) => sys.error("Expected validation error")
    }
  }

  it("accept null as optional") {
    val form = Json.obj(
      "description" -> JsNull,
      "number" -> "1",
      "locale" -> "en_US",
      "name" -> "test",
      "currency" -> "USD",
      "price" -> 10
    )

    validate("item_form", form) match {
      case Left(errors) => sys.error(s"Expected form to validate but got: $errors")
      case Right(_) => // no-op
    }
  }

  it("understands arrays are required") {
    apibuilderMultiService.upcastOperationBody(
      "POST", "/queries", Json.obj()
    ) should equal(
      Left(Seq("Missing required field for query_form: filters"))
    )
  }

}
