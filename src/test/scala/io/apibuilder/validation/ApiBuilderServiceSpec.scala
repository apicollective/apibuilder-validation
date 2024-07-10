package io.apibuilder.validation

import io.apibuilder.validation.helpers.Helpers
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ApiBuilderServiceSpec extends AnyFunSpec with Matchers with Helpers {

  it("fromUrl") {
    ApiBuilderService.fromUrl("file://non-existent-tmp").left.getOrElse {
      sys.error("Expected error from invalid url")
    }

    rightOrErrors {
      ApiBuilderService.toService(readFile("apibuilder-common-service.json"))
    }.service.name should be("apibuilder common")
  }

}
