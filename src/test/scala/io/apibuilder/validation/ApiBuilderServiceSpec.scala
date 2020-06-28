package io.apibuilder.validation

import io.apibuilder.spec.v0.models.Method
import io.apibuilder.validation.helpers.Helpers
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec

class ApiBuilderServiceSpec extends AnyFunSpec with Matchers with Helpers {

  private[this] lazy val service = loadService("flow-api-service.json")

  it("fromUrl") {
    ApiBuilderService.fromUrl("file://non-existent-tmp").left.getOrElse {
      sys.error("Expected error from invalid url")
    }

    rightOrErrors {
      ApiBuilderService.toService(readFile("apibuilder-common-service.json"))
    }.service.name should be("apibuilder common")
  }

  it("operation") {
    service.findOperation(Method.Post, "/foo") should be(None)

    val op = service.findOperation(Method.Post, "/users").get
    op.method should equal(Method.Post)
    op.path should equal("/users")
    op.parameters should be(Nil)

    service.findOperation(Method.Get, "/users").get.parameters.map(_.name) should be(
      Seq("id", "email", "status", "limit", "offset", "sort")
    )
  }
}
