package io.apibuilder.validation

import cats.data.ValidatedNec
import io.apibuilder.builders.ApiBuilderServiceBuilders
import io.apibuilder.helpers.{Helpers, TestHelpers}
import io.apibuilder.spec.v0.models.Service
import org.joda.time.DateTime
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json._

class JsonValidatorSpec extends AnyWordSpec with Matchers with Helpers with TestHelpers with ApiBuilderServiceBuilders {

  private lazy val defaultService: Service = makeService(
    enums = Seq(
      makeEnum("avs_code", values = Seq(
        makeEnumValue("match"),
        makeEnumValue("partial"),
      ))
    ),
    models = Seq(
      makeModel("webhook"),
      makeModel("webhook_form", fields = Seq(
        makeField("url", required = true, `type` = "string"),
        makeField("events", required = true, `type` = "[string]")
      )),
      makeModel("booleans", fields = Seq(
        makeField("value", `type` = "boolean")
      )),
      makeModel("avs", fields = Seq(
        makeField("code", `type` = "avs_code")
      )),
      makeModel("invitation_form", fields = Seq(
        makeField("name", `type` = "object")
      ))
    ),
  )

  private def validate(typ: String, js: JsValue)(implicit service: Service = defaultService): ValidatedNec[String, JsValue] = {
    val svc = ApiBuilderService(service)
    mustFindType(svc, typ)
    JsonValidator(svc).validate(
      typ, js, defaultNamespace = None
    )
  }
  private def validateValid(typ: String, js: JsValue)(implicit service: Service = defaultService): JsValue = {
    expectValidNec {
      validate(typ, js)(service)
    }
  }
  private def validateError(typ: String, js: JsValue)(implicit service: Service = defaultService): Seq[String] = {
    expectInvalidNec {
      validate(typ, js)(service)
    }
  }

  "findType is case insensitive" in {
    val validator = JsonValidator(ApiBuilderService(defaultService))

    val m1 = validator.findType("webhook", defaultNamespace = None).head
    m1.name must equal("webhook")

    val m2 = validator.findType("WEBHOOK", defaultNamespace = None).head
    m2.name must equal("webhook")
  }

  "1 required field" in {
    validateError(
      "webhook_form",
      Json.obj("url" -> "https://test.flow.io")
    ) must equal(
      Seq("Missing required field for webhook_form: events")
    )
  }

  "multiple required fields" in {
    validateError(
      "webhook_form",
      Json.obj()
    ) must equal(
      Seq("Missing required fields for webhook_form: url, events")
    )
  }

  "invalid type" in {
    val form = Json.obj(
      "url" -> Seq("https://a.flow.io", "https://b.flow.io"),
      "events" -> Seq("*")
    )
    validateError("webhook_form", form) must equal(
      Seq(
        "webhook_form.url must be a string and not an array"
      )
    )
  }

  "Upcasts singletons to arrays" in {
    val form = Json.obj(
      "url" -> "https://a.flow.io",
      "events" -> "*"
    )
    validateValid("webhook_form", form) must equal(
      Json.obj(
        "url" -> "https://a.flow.io",
        "events" -> Seq("*")
      )
    )
  }

  "converts number into a string where possible when number is an int" in {
    val form = Json.obj(
      "url" -> 123,
      "events" -> Seq("*")
    )
    validateValid("webhook_form", form) must equal(
      Json.obj(
        "url" -> "123",
        "events" -> Seq("*")
      )
    )
  }

  "converts number into a string where possible when number is a double" in {
    val form = Json.obj(
      "url" -> 123.45,
      "events" -> Seq("*")
    )
    validateValid("webhook_form", form) must equal(
      Json.obj(
        "url" -> "123.45",
        "events" -> Seq("*")
      )
    )
  }

  "validates an integer" in {
    validate("integer", Json.parse("123")).toOption.get.as[Double] must equal(123)
    validateError("integer", Json.parse("123.45")) mustBe Seq("value must be a valid integer")
    validateError("integer", JsString("NaN")) mustBe Seq("value must be a valid integer")
    validateError("integer", JsString(" ")) mustBe Seq("value must be a valid integer")
  }

  "validates a double" in {
    validateValid("double", Json.parse("123.45")).as[Double] must equal(123.45)

    validateValid("double", Json.parse("123")).as[Double] must equal(123)
    validateError("double", JsString("NaN")) mustBe Seq("value must be a valid double")
    validateError("double", JsString(" ")) mustBe Seq("value must be a valid double")
  }

  "validates a decimal" in {
    validate("decimal", Json.parse("123.45")).toOption.get.as[BigDecimal] must equal(123.45)
    validate("decimal", Json.parse("123")).toOption.get.as[BigDecimal] must equal(123)
    validateError("decimal", JsString(" ")) mustBe Seq("value must be a valid decimal")
  }

  "validates a float" in {
    validate("float", Json.parse("123.45")).toOption.get.as[Float] must equal(123.45.toFloat)
    validate("float", Json.parse("123")).toOption.get.as[Float] must equal(123.toFloat)
    validateError("float", JsString(" ")) mustBe Seq("value must be a valid float")
  }

  "validates a UUID" in {
    val uuid = java.util.UUID.randomUUID
    validate("uuid", JsString(uuid.toString)).toOption.get.as[java.util.UUID] must equal(uuid)
    validateError("uuid", JsString(" ")) mustBe Seq("value must be a valid UUID")
  }

  "validates an ISO 8601 date (yyyy-MM-dd)" in {
    validate("date-iso8601", JsString("2017-01-01")).toOption.get.as[String] must equal("2017-01-01")
    validate("date-iso8601", JsString("2017-1-01")).toOption.get.as[String] must equal("2017-1-01")
    validate("date-iso8601", JsString("2017-01-1")).toOption.get.as[String] must equal("2017-01-1")
    validate("date-iso8601", JsString("2017-1-1")).toOption.get.as[String] must equal("2017-1-1")
    validateError("date-iso8601", JsString("invalid")) mustBe Seq("value must be a valid ISO 8601 date. Example: '2017-07-24'")
    // Tests that the format must be yyyy-MM-dd
    validateError("date-iso8601", JsString(new DateTime(2017, 2, 24, 0, 0, 0).toString)) mustBe Seq("value must be a valid ISO 8601 date. Example: '2017-07-24'")
  }

  "validates an ISO 8601 datetime" in {
    val dt = new DateTime(2017, 1, 1, 0, 0, 0).toString
    validate("date-time-iso8601", JsString("2014-06-20T11:41:08+02:00")).toOption.get.as[String] must equal("2014-06-20T11:41:08+02:00")
    validate("date-time-iso8601", JsString("2017-01-01")).toOption.get.as[String] must equal("2017-01-01")
    validate("date-time-iso8601", JsString("2017-1-01")).toOption.get.as[String] must equal("2017-1-01")
    validate("date-time-iso8601", JsString("2017-01-1")).toOption.get.as[String] must equal("2017-01-1")
    validate("date-time-iso8601", JsString("2017-1-1")).toOption.get.as[String] must equal("2017-1-1")
    validate("date-time-iso8601", JsString(dt)).toOption.get.as[String] must equal(dt)
    validateError("date-time-iso8601", JsString("invalid")) mustBe Seq("value must be a valid ISO 8601 datetime. Example: '2017-07-24T09:41:08+02:00'")
  }

  "converts booleans where possible" in {
    (
      Booleans.TrueValues.map(JsString(_)) ++ Seq(JsNumber(1))
    ).foreach { v =>
      validateValid("booleans", Json.obj("value" -> v)) must equal(
        Json.obj(
          "value" -> true
        )
      )
    }

    (
      Booleans.FalseValues.map(JsString(_)) ++ Seq(JsNumber(0))
    ).foreach { v =>
      validateValid("booleans", Json.obj("value" -> v)) must equal(
        Json.obj(
          "value" -> false
        )
      )
    }
  }

  "validates enum values" in {
    val form = Json.obj(
      "code" -> "bad"
    )
    validateError("avs", form) mustBe Seq("avs.code invalid value 'bad'. Valid values for the enum 'avs_code' are: 'match', 'partial'")
  }

  "validates enum values that are passed in as empty strings" in {
    val form = Json.obj(
      "code" -> ""
    )
    validateError("avs", form) mustBe Seq("avs.code invalid value ''. Valid values for the enum 'avs_code' are: 'match', 'partial'")
  }

  "validates nested models" in {
    val service = makeService(
      models = Seq(
        makeModel("card_form", fields = Seq(
          makeField("address", `type` = "address_form")
        )),
        makeModel("address_form", fields = Seq(
          makeField("streets", `type` = "[string]")
        )),
      )
    )

    val form = Json.obj(
      "address" -> Json.obj(
        "streets" -> JsArray(Seq(JsString("1 main st"), JsNull))
      )
    )

    expectInvalidNec {
      validate("card_form", form)(service)
    } mustBe Seq("card_form.address.streets of type '[string]': element in position[1] must be a string and not null")
  }

  "returns appropriate error messages from maps" in {
    val service = makeService(
      models = Seq(
        makeModel("item_form", fields = Seq(
          makeField("attributes", `type` = "map[string]")
        ))
      )
    )

    validateError("item_form", Json.obj(
      "attributes" -> Json.obj(
        "a" -> Json.obj()
      )
    ))(service) mustBe Seq("item_form.attributes of type 'map[string]': element[a] must be a string and not an object")
  }

  "Properly reports errors on object" in {
    val service = makeService(
      models = Seq(
        makeModel("invitation_form", fields = Seq(
          makeField("name", `type` = "object")
        ))
      )
    )

    validateError("invitation_form", Json.obj(
      "name" -> "",
    ))(service) mustBe Seq("invitation_form.name must be an object and not a string")
  }

  "converts array" must {
    val service = makeService(
      models = Seq(makeModel("order", fields = Seq(
        makeField("number", `type` = "string")
      )))
    )

    "length 1 into a single value" in {
      // This supports a use case from python libraries that send single values w/ array syntax (e.g. number[]=1)

      expectValidNec {
        validate("order", Json.obj(
          "number" -> Seq[String]("123")
        ))(service)
      } must equal(
        Json.obj(
          "number" -> "123"
        )
      )
    }

    "reports errors when expecting a single value and presented with an array of length > 1" in {
      expectInvalidNec {
        validate("order", Json.obj(
          "number" -> Seq[String]("123", "456")
        ))(service)
      } mustBe Seq("order.number must be a string and not an array")
    }
  }

  "accept null as optional" in {
    val service = makeService(
      models = Seq(
        makeModel("user", fields = Seq(
          makeField("description", required = false)
        ))
      )
    )

    expectValidNec {
      validate("user", Json.obj(
        "description" -> JsNull
      ))(service)
    }
  }

  "understands arrays are required" in {
    val service = makeService(
      models = Seq(
        makeModel("user", fields = Seq(
          makeField("emails", `type` = "[string]")
        ))
      )
    )

    validateError("user", Json.obj())(service) mustBe Seq("Missing required field for user: emails")
  }

  "integer" in {
    val service = makeService(
      models = Seq(
        makeModel("user", fields = Seq(
          makeField("birth_year", `type` = "integer")
        ))
      )
    )

    validateError("user", Json.obj(
      "birth_year" -> "a"
    ))(service) mustBe Seq("user.birth_year must be a valid integer")
  }

  "properly identifies invalid decimal" in {
    val service = makeService(
      models = Seq(makeModel("price", fields = Seq(
        makeField("amount", `type` = "decimal"),
        makeField("currency"),
      )))
    )
    validateError("price", Json.obj(
        "amount" -> "a",
        "currency" -> "USD"
    ))(service) mustBe Seq("price.amount must be a valid decimal")
  }
}
