package io.apibuilder.validation.helpers

import io.apibuilder.validation.{AnyType, ApiBuilderType, MultiService}

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
