package io.apibuilder.validation

import cats.data.ValidatedNec
import cats.implicits._
import io.apibuilder.spec.v0.models.{Method, Operation, Service}
import io.apibuilder.validation.util.StandardErrors

object PathNormalizer {

  def apply(apiBuilderService: ApiBuilderService): PathNormalizer = {
    apply(apiBuilderService.service)
  }

  def apply(multi: MultiService): PathNormalizer = {
    apply(
      multi.services.flatMap(_.service.resources.flatMap(_.operations))
    )
  }

  def apply(service: Service): PathNormalizer = {
    PathNormalizer(service.resources.flatMap(_.operations))
  }

}

/**
  * Normalizes paths based on variables defined in the path.
  * For example, a path like "/users/123" would resolve to "/users/:id"
  * assuming there is a valid operation for that path.
  *
  * Strategy:
  *   - Use a hash map lookup for all static routes (no variables)
  *   - For paths with variables
  *     - First segment by the HTTP Method
  *     - Iterate through list to call matches on each route
  */
case class PathNormalizer(operations: Seq[Operation]) {

  /**
    * Create two indexes of the routes:
    *   - static routes are simple lookups by path (Map[String, Route])
    *   - dynamic routes is a map from the HTTP Method to a list of routes to try (Seq[Route])
    */
  private val (staticRouteMap: StaticRouteMap, dynamicRouteMap: DynamicRouteMap) = {
    val opsWithRoute = operations.map { op => OperationWithRoute(op) }

    // Map from method name to list of operations
    val (staticRoutes, dynamicRoutes) = opsWithRoute.partition { opWithRoute =>
      opWithRoute.route match {
        case _: Route.Static => true
        case _: Route.Dynamic => false
      }
    }

    (
      StaticRouteMap(staticRoutes),
      DynamicRouteMap(dynamicRoutes)
    )
  }

  final def resolve(method: Method, path: String): ValidatedNec[String, Operation] = {
    method match {
      case Method.UNDEFINED(m) => {
        StandardErrors.invalidMethodError(m).invalidNec
      }
      case _ => {
        staticRouteMap.find(method, path) match {
          case Some(opWithRoute) => {
            opWithRoute.op.validNec
          }
          case None => {
            dynamicRouteMap.find(method, path) match {
              case None => s"HTTP Operation '$method $path' is not defined".invalidNec
              case Some(route) => route.op.validNec
            }
          }
        }
      }
    }
  }
}

private[validation] case class OperationWithRoute(
  op: Operation,
  route: Route
)

private[validation] object OperationWithRoute {

  def apply(op: Operation): OperationWithRoute = {
    OperationWithRoute(
      op = op,
      route = Route(op)
    )
  }

}
