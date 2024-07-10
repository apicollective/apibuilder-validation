package io.apibuilder.validation

import io.apibuilder.builders.ApiBuilderServiceBuilders
import io.apibuilder.helpers.{Helpers, TestHelpers}
import io.apibuilder.spec.v0.models.Method
import play.api.libs.json.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MultiServiceImplSpec extends AnyWordSpec with Matchers with Helpers with TestHelpers with ApiBuilderServiceBuilders {

  private lazy val multi = MultiServiceImpl(
    List(
      ApiBuilderService(makeService(
        name = "API",
        models = Seq(
          makeModel("user"),
          makeModel("user_form"),
          makeModel("organization"),
          makeModel("webhook"),
          makeModel("webhook_form"),
        ),
        resources = Seq(
          makeResource("user", operations = Seq(
            makeOperation(Method.Get, "/users", parameters = Seq(makeParameter("id"))),
            makeOperation(Method.Post, "/users", body = Some(makeBody("user_form"))),
          )),
          makeResource("webhook", operations = Seq(
            makeOperation(Method.Post, "/:organization/webhooks", body = Some(makeBody("webhook_form")))
          ))
        ))
      ),
      ApiBuilderService(makeService(
        name = "Spec",
        models = Seq(
          makeModel("application"),
          makeModel("application_form"),
        ),
        resources = Seq(
          makeResource("application", operations = Seq(
            makeOperation(Method.Post, "/:orgKey", body = Some(makeBody("application_form")))
          ))
        )
      )),
    )
  )

  "loads multiple services" in {
    multi.services.map(_.service.name) mustBe Seq("API", "Spec")
  }

  "bodyTypeFromPath" in {
    multi.bodyTypeFromPath("POST", "/unknown/path/that/does/not/resolve") must equal(None)

    // resources from flow api
    multi.bodyTypeFromPath("POST", "/users").map(_.name) must equal(Some("user_form"))
    multi.bodyTypeFromPath("POST", "/:organization/webhooks").map(_.name) must equal(Some("webhook_form"))

    // resources from apidoc api
    multi.bodyTypeFromPath("POST", "/:orgKey").map(_.name) must equal(Some("application_form"))
  }

  "operation" in {
    multi.findOperation(Method.Post, "/foo/bar/baz") must be(None)

    val op = multi.findOperation("POST", "/users").get.operation
    op.method must equal(Method.Post)
    op.path must equal("/users")
    op.parameters must be(Nil)

    multi.findOperation("GET", "/users").get.operation.parameters.map(_.name) must be(
      Seq("id")
    )
  }
/*
  "validate" in {
    // path from flow api
    multi.upcastOperationBody(
      "POST",
      "/:organization/webhooks",
      Json.obj("url" -> "https://test.flow.io")
    ) must equal(
      Left(Seq("Missing required field for webhook_form: events"))
    )

    // path from apidoc api
    multi.upcastOperationBody(
      "POST",
      "/:orgKey",
      Json.obj("url" -> "https://test.flow.io")
    ) must equal(
      Left(Seq("Missing required fields for application_form: name, visibility"))
    )
  }

  "error on missing array indices" in {
    // comes from decoding customer[address][streets][]=33b Bay St
    multi.upcastOperationBody(
      "PUT",
      "/:organization/orders/:number",
      Json.parse(
        """
          |{
          |    "items": [],
          |    "customer": {
          |        "name": {
          |            "first": "Matt",
          |            "last": "Flow"
          |        },
          |        "phone": "1234567890",
          |        "email": "mkersner@flow.io",
          |        "address": {
          |            "name": {
          |                "first": "Matt",
          |                "last": "Flow"
          |            },
          |            "streets": {
          |                "": "33b Bay St"
          |            },
          |            "city": "Toronto",
          |            "province": "Ontario",
          |            "postal": "M5J 2Z3",
          |            "country": "CAN",
          |            "company": "Flow"
          |        }
          |    }
          |}
        """.stripMargin)
    ) must equal(
      Left(Seq("order_put_form.customer.address.streets of type '[string]': element in position[0] must be a string and not an object"))
    )
  }

  "url query example" in {
    val form = FormData.parseEncoded("name=John%20Doe&expiration_month=12&expiration_year=2017&cvv=123&cipher=VnHzBw%2BbaGrKZL0fimklhKupHJeowxK2Mqa9LbECCnb3R%2FxIgS1vr0sFg2mUGsXR7bsNV61UURB91VrWr19V1g%3D%3D&challenge%5Btext%5D=Flow&challenge%5Bcipher%5D=df2BQZykhnTfIVIX6Vg9yjUmyEprz3dLmUYU0O8GeyCZ0t3pn1nXSP7DRDfsZAASwtNupqyYx3G4W%2BmGlWQreg%3D%3D&callback=__flowjsonp0&method=post")

    val js = multi.upcastOperationBody(
      "POST",
      "/:organization/cards",
      Json.toJson(form)
    ).toOption.get

    (js \ "name").as[JsString].value must equal("John Doe")
    (js \ "expiration_month").as[JsNumber].value must equal(12)
    (js \ "expiration_year").as[JsNumber].value must equal(2017)
    (js \ "cvv").as[JsString].value must equal("123")
  }

  "simple number example" in {
    val form = Json.obj(
      "url" -> 123,
      "events" -> 456
    )

    val js = multi.upcastOperationBody(
      "POST",
      "/:organization/webhooks",
      Json.toJson(form)
    ).toOption.get

    (js \ "url").as[JsString].value must equal("123")
    (js \ "events").as[JsArray] must equal(JsArray(Seq(JsString("456"))))
  }

  "offers validation error w/ verb replacement" in {
    multi.upcastOperationBody(
      "OPTIONS",
      "/:organization/webhooks",
      Json.obj("url" -> "https://test.flow.io", "events" -> "*")
    ) must equal(
      Left(Seq(
        "HTTP method 'OPTIONS' not defined for path '/:organization/webhooks' - Available methods: GET, POST, PUT, DELETE"
      ))
    )
  }

  "validate union type discriminator" in {
    multi.upcastOperationBody(
      "POST",
      "/:organization/authorizations",
      Json.obj("discriminator" -> "authorization_form")
    ) match {
      case Left(errors) => errors.head.contains("Invalid discriminator 'authorization_form'") must be(true)
      case Right(_) => sys.error("Expected error")
    }
  }

  "validate union type" in {
    multi.upcastOperationBody(
      "POST",
      "/:organization/authorizations",
      Json.obj(
        "order_number" -> "123",
        "discriminator" -> "merchant_of_record_authorization_form"
      )
    ) must equal(
      Left(Seq("Missing required field for merchant_of_record_authorization_form: token"))
    )
  }
*/
}
