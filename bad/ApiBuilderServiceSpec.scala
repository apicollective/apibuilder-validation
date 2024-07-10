package io.apibuilder.validation

import io.apibuilder.builders.ApiBuilderServiceBuilders
import io.apibuilder.helpers.{ApiBuilderServiceValidatorHelpers, TestHelpers}
import io.apibuilder.validation.helpers.{FileHelpers, Helpers}
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.Json
import io.apibuilder.spec.v0.models.json._

class ApiBuilderServiceSpec extends AnyWordSpec with Matchers with Helpers with FileHelpers with TestHelpers with ApiBuilderServiceBuilders {

  "from non existent url" must {
    "invalid" in {
      expectInvalidNec {
        ApiBuilderService.fromUrl(s"file://${randomString()}")
      } mustBe Seq("TODO")
    }

    "valid" in {
      val service = makeService()
      val file = writeToTempFile(Json.toJson(service))
      expectValid {
        ApiBuilderService.toService(readFile(s"file://${file.getAbsolutePath}))
      }
    }
  }

}
