package io.apibuilder.validation.helpers

import java.io.File
import io.apibuilder.spec.v0.models.Service
import io.apibuilder.spec.v0.models.json._
import io.apibuilder.validation.zip.FileUtil
import io.apibuilder.validation.{AnyType, ApiBuilderService, ApiBuilderType, MultiService, MultiServiceImpl}
import play.api.libs.json.Json

trait Helpers {

  def mustFindType(multiService: MultiService, qualified: String): AnyType = {
    multiService.findType(qualified).getOrElse {
      sys.error(s"Failed to find type '$qualified")
    }
  }

  def mustFindModelType(multiService: MultiService, qualified: String): ApiBuilderType.Model = {
    mustFindType(multiService, qualified).asInstanceOf[ApiBuilderType.Model]
  }

}
