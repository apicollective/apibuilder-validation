package io.apibuilder.helpers

import io.apibuilder.validation.{AnyType, ApiBuilderService, ApiBuilderType, MultiService}

trait Helpers {

  def mustFindType(service: ApiBuilderService, typeName: String): AnyType = {
    MultiService(List(service)).findType(service.namespace, typeName).getOrElse {
      sys.error(s"Failed to find type '$typeName'")
    }
  }

  def mustFindType(multiService: MultiService, qualified: String): AnyType = {
    multiService.findType(qualified).getOrElse {
      sys.error(s"Failed to find type '$qualified'")
    }
  }

  def mustFindModelType(multiService: MultiService, qualified: String): ApiBuilderType.Model = {
    mustFindType(multiService, qualified).asInstanceOf[ApiBuilderType.Model]
  }

}
