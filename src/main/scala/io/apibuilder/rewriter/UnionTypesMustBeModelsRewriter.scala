package io.apibuilder.rewriter

import apibuilder.{ApiBuilderHelper, ApiBuilderHelperImpl}
import io.apibuilder.validation.{ApiBuilderType, MultiService, ScalarType}

/**
 * For any union that contains a type that is NOT a model, remove it
 * and replace any references to it with "object"
 */
object UnionTypesMustBeModelsRewriter extends DefaultRewriter {

  override def rewrite(multiService: MultiService): MultiService = {
    val helper = ApiBuilderHelperImpl(multiService)
    val toRemoveNames = multiService.allUnions.filterNot { u =>
      areAllTypesModels(helper, u)
    }.map(_.qualified).toSet

    val ms = TypeRewriter {
      case t: ApiBuilderType if toRemoveNames.contains(t.qualified) => {
        ScalarType.ObjectType
      }
      case t: ApiBuilderType => t
      case t: ScalarType => t
    }.rewrite(multiService)

    removeUnions(ms, toRemoveNames)
  }

  private def removeUnions(multiService: MultiService, toRemoveNames: Set[String]): MultiService = {
    MultiService(
      multiService.services.map { s =>
        s.copy(
          service = s.service.copy(
            unions = s.unions.filterNot { u => toRemoveNames.contains(u.qualified) }.map(_.union),
          )
        )
      }
    )

  }

  private def areAllTypesModels(helper: ApiBuilderHelper, union: ApiBuilderType.Union): Boolean = {
    union.union.types.forall { t =>
      helper.resolveType(union.service, t.`type`) match {
        case Some(_: ApiBuilderType.Model) => true
        case _ => false
      }
    }
  }

}
