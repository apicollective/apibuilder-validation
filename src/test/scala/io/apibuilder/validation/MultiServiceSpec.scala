package io.apibuilder.validation

import io.apibuilder.builders.ApiBuilderServiceBuilders
import io.apibuilder.helpers.{Helpers, TestHelpers}
import io.apibuilder.spec.v0.models.{Method, ResponseCodeInt, Operation}
import play.api.libs.json._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MultiServiceSpec extends AnyWordSpec with Matchers with Helpers with TestHelpers with ApiBuilderServiceBuilders {

  private val defaultService = MultiService(List(ApiBuilderService(makeService())))

  "validates unknown method" in {
    val service = MultiService(List(ApiBuilderService(makeService(
      models = Seq(
        makeModel("payment"),
        makeModel("payment_form"),
      ),
      resources = Seq(makeResource("payment", operations = Seq(
        makeOperation(Method.Post, "/:organization/payments", body = Some(makeBody("payment_form")))
      )))
    ))))
    expectInvalidNec {
      service.validateOperation(Method.UNDEFINED("FOO"), "/:organization/payments")
    } mustBe Seq("HTTP method 'FOO' is invalid. Must be one of: " + Method.all.map(_.toString).mkString(", "))

    expectInvalidNec {
      service.validateOperation(Method.Options, "/:organization/payments")
    } mustBe Seq("HTTP method 'OPTIONS' not defined for path '/:organization/payments' - Available methods: POST")
  }

  "validates unknown paths" in {
    expectInvalidNec {
      defaultService.validateOperation(Method.Get, "/foo")
    } mustBe Seq("HTTP path '/foo' is not defined")
  }

  "findType can resolve a scalar" in {
    defaultService.findType("string").get must equal(ScalarType.StringType)
    defaultService.findType("STRING").get must equal(ScalarType.StringType)
    ScalarType.all.forall { t =>
      defaultService.findType(t.name).isDefined
    } must be(true)
  }

  "resolves body when path exists in both services" in {
    def buildService: ApiBuilderService = {
      ApiBuilderService(makeService(
        models = Seq(
          makeModel("payment"),
          makeModel("payment_form"),
        ),
        resources = Seq(makeResource("payment", operations = Seq(
          makeOperation(Method.Post, "/payments/:key/other/:foo", body = Some(makeBody("payment_form")))
        )))
      ))
    }

    MultiService(List(
      buildService,
      buildService,
    )).bodyTypeFromPath("POST", "/payments/:key/other/:foo").get.name must equal("payment_form")
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
  "validates when path exists in both services with different available methods" in {
    def buildService(op: Option[Operation]): ApiBuilderService = ApiBuilderService(makeService(
      models = Seq(
        makeModel("payment")
      ),
      resources = Seq(makeResource("payment", operations = Seq(
        makeOperation(Method.Get, "/:organization/payments")
      ) ++ op.toSeq))
    ))

    expectValidNec {
      MultiService(List(
        buildService(None),
        buildService(Some(makeOperation(Method.Post, "/:organization/payments"))),
      )).upcastOperationBody(
        "POST",
        "/:organization/payments",
        Json.obj()
      )
    }
  }

  "responses" must {
    val multiService = MultiService(List(ApiBuilderService(makeService(
      models = Seq(
        makeModel("card", fields = Seq(
          makeField("id", required = true),
          makeField("parent", required = false),
        ))
      ),
      resources = Seq(makeResource("card", operations = Seq(
        makeOperation(Method.Post, "/cards", responses = Seq(
          makeResponse(ResponseCodeInt(201), `type` = "card"),
          makeResponse(ResponseCodeInt(401)),
          makeResponse(ResponseCodeInt(422))
        ))
      )))
    ))))

    "code" in {
      val op = multiService.findOperation("POST", "/cards").get

      Seq(201, 401, 422).foreach { code =>
        expectValidNec {
          multiService.validateResponseCode(op.operation, code)
        }
      }

      Seq(100, 200, 417, 500, 503).foreach { code =>
        expectInvalidNec {
          multiService.validateResponseCode(op, code)
        } mustBe Seq(s"Unexpected response code[$code] for operation[POST /cards]. Declared response codes: 201, 401, 422")
      }
    }

    "response type" in {
      val op = multiService.findOperation("POST", "/cards").get
      multiService.response(op, 201).get.`type` must equal("card")
      multiService.response(op, 499) must be(None)
    }

    "correctly parses required fields" in {
      val orgModel = multiService.allModels.find(_.name == "card").get.model
      orgModel.fields.find(_.name == "id").get.required must be(true)
      orgModel.fields.find(_.name == "parent").get.required must be(false)
    }
  }
}