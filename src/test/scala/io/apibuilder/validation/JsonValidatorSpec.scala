package io.apibuilder.validation

import cats.data.ValidatedNec
import io.apibuilder.builders.ApiBuilderServiceBuilders
import io.apibuilder.helpers.TestHelpers
import io.apibuilder.validation.helpers.Helpers
import org.joda.time.DateTime
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.*

class JsonValidatorSpec extends AnyWordSpec with Matchers with Helpers with TestHelpers with ApiBuilderServiceBuilders {

  private lazy val validator: JsonValidator = JsonValidator(
    ApiBuilderService(
      makeService(
        models = Seq(
          makeModel("webhook"),
          makeModel("webhook_form", fields = Seq(
            makeField("url", required = true, `type` = "string"),
            makeField("events", required = true, `type` = "[string]")
          )),
        ),
      )
    )
  )

  private def validate(typ: String, js: JsValue): ValidatedNec[String, JsValue] = {
    validator.validate(
      typ, js, defaultNamespace = None
    )
  }
  private def validateError(typ: String, js: JsValue): Seq[String] = {
    expectInvalidNec {
      validate(typ, js)
    }
  }

  "findType is case insensitive" in {
    val m1 = validator.findType("webhook", defaultNamespace = None).head
    m1.name must equal("webhook")

    val m2 = validator.findType("WEBHOOK", defaultNamespace = None).head
    m2.name must equal("webhook")
  }

  "1 required field" in {
    validate(
      "webhook_form",
      Json.obj("url" -> "https://test.flow.io")
    ) must equal(
      Left(Seq("Missing required field for webhook_form: events"))
    )
  }

  "multiple required fields" in {
    validate(
      "webhook_form",
      Json.obj()
    ) must equal(
      Left(Seq("Missing required fields for webhook_form: url, events"))
    )
  }

  "invalid type" in {
    val form = Json.obj(
      "url" -> Seq("https://a.flow.io", "https://b.flow.io"),
      "events" -> Seq("*")
    )
    validate("webhook_form", form) must equal(
      Left(
        Seq(
          "webhook_form.url must be a string and not an array"
        )
      )
    )
  }

  "Upcasts singletons to arrays" in {
    val form = Json.obj(
      "url" -> "https://a.flow.io",
      "events" -> "*"
    )
    validate("webhook_form", form) must equal(
      Right(
        Json.obj(
          "url" -> "https://a.flow.io",
          "events" -> Seq("*")
        )
      )
    )
  }

  "converts number into a string where possible when number is an int" in {
    val form = Json.obj(
      "url" -> 123,
      "events" -> Seq("*")
    )
    validate("webhook_form", form) must equal(
      Right(
        Json.obj(
          "url" -> "123",
          "events" -> Seq("*")
        )
      )
    )
  }

  "converts number into a string where possible when number is a double" in {
    val form = Json.obj(
      "url" -> 123.45,
      "events" -> Seq("*")
    )
    validate("webhook_form", form) must equal(
      Right(
        Json.obj(
          "url" -> "123.45",
          "events" -> Seq("*")
        )
      )
    )
  }

  "validates an integer" in {
    validate("integer", Json.parse("123")).toOption.get.as[Double] must equal(123)
    validateError("integer", Json.parse("123.45")) mustBe Seq("value must be a valid integer")
    validate("integer", JsString("NaN")) must equal(Left(List("value must be a valid integer")))
    validate("integer", JsString(" ")) must equal(Left(List("value must be a valid integer")))
  }

  "validates a double" in {
    validate("double", Json.parse("123.45")).toOption.get.as[Double] must equal(123.45)
    validate("double", Json.parse("123")).toOption.get.as[Double] must equal(123)
    validate("double", JsString("NaN")) must equal(Left(List("value must be a valid double")))
    validate("double", JsString(" ")) must equal(Left(List("value must be a valid double")))
  }

  "validates a decimal" in {
    validate("decimal", Json.parse("123.45")).toOption.get.as[BigDecimal] must equal(123.45)
    validate("decimal", Json.parse("123")).toOption.get.as[BigDecimal] must equal(123)
    validate("decimal", JsString(" ")) must equal(Left(List("value must be a valid decimal")))
  }

  "validates a UUID" in {
    val uuid = java.util.UUID.randomUUID
    validate("uuid", JsString(uuid.toString)).toOption.get.as[java.util.UUID] must equal(uuid)
    validate("uuid", JsString(" ")) must equal(Left(List("value must be a valid UUID")))
  }

  "validates an ISO 8601 date (yyyy-MM-dd)" in {
    validate("date-iso8601", JsString("2017-01-01")).toOption.get.as[String] must equal("2017-01-01")
    validate("date-iso8601", JsString("2017-1-01")).toOption.get.as[String] must equal("2017-1-01")
    validate("date-iso8601", JsString("2017-01-1")).toOption.get.as[String] must equal("2017-01-1")
    validate("date-iso8601", JsString("2017-1-1")).toOption.get.as[String] must equal("2017-1-1")
    validate("date-iso8601", JsString("invalid")) must equal(Left(List("value must be a valid ISO 8601 date. Example: '2017-07-24'")))
    // Tests that the format must be yyyy-MM-dd
    validate("date-iso8601", JsString(new DateTime(2017, 2, 24, 0, 0, 0).toString)) must equal(Left(List("value must be a valid ISO 8601 date. Example: '2017-07-24'")))
  }

  "validates an ISO 8601 datetime" in {
    val dt = new DateTime(2017, 1, 1, 0, 0, 0).toString
    validate("date-time-iso8601", JsString("2014-06-20T11:41:08+02:00")).toOption.get.as[String] must equal("2014-06-20T11:41:08+02:00")
    validate("date-time-iso8601", JsString("2017-01-01")).toOption.get.as[String] must equal("2017-01-01")
    validate("date-time-iso8601", JsString("2017-1-01")).toOption.get.as[String] must equal("2017-1-01")
    validate("date-time-iso8601", JsString("2017-01-1")).toOption.get.as[String] must equal("2017-01-1")
    validate("date-time-iso8601", JsString("2017-1-1")).toOption.get.as[String] must equal("2017-1-1")
    validate("date-time-iso8601", JsString(dt)).toOption.get.as[String] must equal(dt)
    validate("date-time-iso8601", JsString("invalid")) must equal(Left(List("value must be a valid ISO 8601 datetime. Example: '2017-07-24T09:41:08+02:00'")))
  }

  "converts booleans where possible" in {
    (
      Booleans.TrueValues.map(JsString(_)) ++ Seq(JsNumber(1))
    ).foreach { v =>
      val form = Json.obj(
        "code" -> "match",
        "name" -> v
      )
      validate("avs", form) must equal(
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
      validate("avs", form) must equal(
        Right(
          Json.obj(
            "code" -> "match",
            "name" -> false
          )
        )
      )
    }
  }

  "validates enum values" in {
    val form = Json.obj(
      "code" -> "bad"
    )
    validate("avs", form) must equal(
      Left(
        Seq("avs.code invalid value 'bad'. Valid values for the enum 'avs_code' are: 'match', 'partial', 'unsupported', 'no_match'")
      )
    )
  }

  "validates enum values that are passed in as empty strings" in {
    val form = Json.obj(
      "code" -> ""
    )
    validate("avs", form) must equal(
      Left(
        Seq("avs.code invalid value ''. Valid values for the enum 'avs_code' are: 'match', 'partial', 'unsupported', 'no_match'")
      )
    )
  }

  "validates nested models" in {
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

    expectInvalidNec {
      validate("card_form", form)
    } mustBe Seq("card_form.address.streets of type '[string]': element in position[1] must be a string and not null")
  }

  "returns appropriate error messages from maps" in {
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

    validate("item_form", form) must be(
      Left(
        Seq("item_form.attributes of type 'map[string]': element[a] must be a string and not an object")
      )
    )
  }

  "Properly reports errors on js objects" in {
    val form = Json.obj(
      "name" -> "",
      "email" -> "test-user@test.flow.io",
      "organization" -> "demo",
      "role" -> "member"
    )

    expectInvalidNec {
      validate("invitation_form", form)
    } mustBe Seq("invitation_form.name must be an object and not a string")
  }

  "converts array of length 1 into a single value" in {
    // This supports a use case from python libraries that send single values w/ array syntax (e.g. number[]=1)
    val form = Json.obj(
      "order_number" -> Seq[String]("123"),
      "token" -> "blah",
      "discriminator" -> "merchant_of_record_authorization_form"
    )

    expectValidNec {
      validate("merchant_of_record_authorization_form", form)
    } must equal(
      Json.obj(
        "order_number" -> "123",
        "token" -> "blah",
        "discriminator" -> "merchant_of_record_authorization_form"
      )
    )
  }

  "reports errors when expecting a single value and presented with an array of length > 1" in {
    val form = Json.obj(
      "order_number" -> Seq[String]("123", "456"),
      "token" -> "blah",
      "discriminator" -> "merchant_of_record_authorization_form"
    )

    expectInvalidNec {
      validate("merchant_of_record_authorization_form", form)
    } mustBe Seq("merchant_of_record_authorization_form.order_number must be a string and not an array")
  }

  "accept null as optional" in {
    val form = Json.obj(
      "description" -> JsNull,
      "number" -> "1",
      "locale" -> "en_US",
      "name" -> "test",
      "currency" -> "USD",
      "price" -> 10
    )

    expectValidNec {
      validate("item_form", form)
    }
  }
/*
  "understands arrays are required" in {
    apiBuilderMultiService.upcastOperationBody(
      "POST", "/queries", Json.obj()
    ) must equal(
      Left(Seq("Missing required field for query_form: filters"))
    )
  }

  "expiration year" in {
    val expiration = apiBuilderMultiService.findType("io.apibuilder.explicit.validation.v0.models.expiration").getOrElse {
      sys.error("Did not find 'expiration' model")
    }
    apiBuilderMultiService.upcast(
      expiration, Json.obj(
        "month" -> 1,
        "year" -> "a",
      )
    ) must equal(
      Left(List("expiration.year must be a valid integer"))
    )
  }
*/
}
