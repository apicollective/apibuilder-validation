package io.apibuilder.rewriter

import io.apibuilder.validation.{ApiBuilderService, MultiService}

trait MultiServiceRewriter {

  def rewrite(multiService: MultiService): MultiService

}

trait ApiBuilderServiceRewriter {

  def rewrite(apiBuilderService: ApiBuilderService): ApiBuilderService

}

trait DefaultRewriter extends MultiServiceRewriter with ApiBuilderServiceRewriter {

  override final def rewrite(apiBuilderService: ApiBuilderService): ApiBuilderService = {
    rewrite(MultiService(List(apiBuilderService))).services.head
  }

}

