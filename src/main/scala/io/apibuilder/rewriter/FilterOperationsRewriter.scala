package io.apibuilder.rewriter

import io.apibuilder.spec.v0.models.{Operation, Resource}
import io.apibuilder.validation.{ApiBuilderService, MultiService}

/**
 * Rewrite the resources and operations based on an accepts filter.
 * If the corresponding resource has no operations, it is removed.
 */
case class FilterOperationsRewriter(
  rewriteOperation: Operation => Option[Operation],
) extends MultiServiceRewriter with ApiBuilderServiceRewriter {

  override def rewrite(multiService: MultiService): MultiService = {
    MultiService(
      multiService.services().map(rewrite)
    )
  }

  override def rewrite(apiBuilderService: ApiBuilderService): ApiBuilderService = {
    apiBuilderService.copy(
      service = apiBuilderService.service.copy(
        resources = apiBuilderService.service.resources.flatMap(filter),
      )
    )
  }

  private def filter(resource: Resource): Option[Resource] = {
    resource.operations.flatMap(rewriteOperation).toList match {
      case Nil => None
      case ops => Some(resource.copy(operations = ops))
    }
  }

}
