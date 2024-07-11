package io.apibuilder.validation

import io.apibuilder.builders.ApiBuilderServiceBuilders
import io.apibuilder.helpers.{ApiBuilderServiceValidatorHelpers, FileHelpers, Helpers, TestHelpers}
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.Json
import io.apibuilder.spec.v0.models.json._

class ApiBuilderServiceSpec extends AnyWordSpec with Matchers with Helpers with FileHelpers with TestHelpers with ApiBuilderServiceBuilders {

  "from non existent url" must {
    "invalid" in {
      val url = s"file://${randomString()}"
      expectInvalidNec {
        ApiBuilderService.fromUrl(url)
      }.head.contains("Error downloading URL") mustBe true
    }

    "valid" in {
      val service = makeService()
      val file = writeToTempFile(Json.toJson(service).toString)
      expectValid {
        ApiBuilderService.fromUrl(s"file://${file.getAbsolutePath}")
      }
    }
  }

}
