package io.apibuilder.rewriter

import io.apibuilder.validation.{ApiBuilderType, MultiService, ScalarType}

// Inspired by UnionTypesMustBeModelsRewriter
object InterfaceTypesRewriter extends MultiServiceRewriter {
  override def rewrite(multiService: MultiService): MultiService = {
    val interfaceNames = multiService.allInterfaces.map(_.qualified).toSet

    val rewritten = TypeRewriter {
      case t: ApiBuilderType if interfaceNames.contains(t.qualified) => ScalarType.ObjectType
      case t: ApiBuilderType => t
      case t: ScalarType => t
    }.rewrite(multiService)

    rewritten
  }
}