package io.apibuilder.validation

import io.apibuilder.spec.v0.models.Method
import play.api.libs.json._
import org.scalatest.{FunSpec, Matchers}

class MultiServiceSpec2 extends FunSpec with Matchers with helpers.Helpers {

  it("validates unknown operations") {
    flowMultiService.validateOperation(Method.UNDEFINED("FOO"), "/:organization/payments") should equal(
      Left(Seq("HTTP method 'FOO' is invalid. Must be one of: " + Method.all.map(_.toString).mkString(", ")))
    )

    flowMultiService.validateOperation(Method.Options, "/:organization/payments") should equal(
      Left(Seq("HTTP method 'OPTIONS' not defined for path '/:organization/payments' - Available methods: GET, POST"))
    )
  }

  it("validates unknown paths") {
    flowMultiService.validateOperation(Method.Get, "/foo") should equal(
      Left(Seq("HTTP path '/foo' is not defined"))
    )
  }

  it("resolves body when path exists in both services") {
    flowMultiService.bodyTypeFromPath("POST", "/:organization/payments").map(_.name) should equal(Some("payment_form"))
  }

  it("resolves body when there are multiple variables in path") {
    flowMultiService.bodyTypeFromPath("POST", "/:organization/shipping/configuration/copies/:key").map(_.name) should equal(
      Some("shipping_configuration_copy_form")
    )
  }

  /**
    * This test has two services that define methods like:
    *
    * Service1:
    *   - GET /:organization/payments
    *   - POST /:organization/payments
    *
    * Service2:
    *   - GET /:organization/payments
    *
    * We are testing that we can correctly resolve the POST, fixing a bug where
    * we were propagating the fact that service2 does not define POST through the
    * validation methods (vs. correctly resolving service 1)
    */
  it("validates when path exists in both services with different available methods") {
    rightOrErrors(
      flowMultiService.upcastOperationBody(
        "POST",
        "/:organization/payments",
        Json.obj(
          "discriminator" -> "merchant_of_record_payment_form",
          "method" -> "paypal",
          "order_number" -> "F1001",
          "amount" -> 1.00,
          "currency" -> "CAD"
        )
      )
    )
  }

  it("validateResponseCode") {
    val op = flowMultiService.findOperation("POST", "/:organization/payments").get

    Seq(201, 401, 422).foreach { code =>
      rightOrErrors {
        flowMultiService.validateResponseCode(op.operation, code)
      }
    }

    Seq(100, 200, 417, 500, 503).foreach { code =>
      flowMultiService.validateResponseCode(op, code) match {
        case Left(error) => error should equal(
          s"Unexpected response code[$code] for operation[POST /:organization/payments]. Declared response codes: 201, 401, 403, 422"
        )
        case Right(v) => sys.error(s"Expected error but got: $v")
      }
    }
  }

  it("response") {
    val op = flowMultiService.findOperation("POST", "/:organization/cards").get
    flowMultiService.response(op, 201).get.`type` should equal("card")
    flowMultiService.response(op, 499) should be(None)
  }

  it("correctly parses required fields") {
    val orgModel = mustFindModelType(flowMultiService, "io.flow.v0.models.organization").model
    orgModel.fields.find(_.name == "id").get.required should be(true)
    orgModel.fields.find(_.name == "parent").get.required should be(false)
  }

}
