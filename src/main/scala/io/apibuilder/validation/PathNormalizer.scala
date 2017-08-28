package io.apibuilder.validation

import io.apibuilder.spec.v0.models.{Method, Operation, Service}

object PathNormalizer {

  def apply(apiBuilderService: ApiBuilderService): PathNormalizer = {
    apply(apiBuilderService.service)
  }

  def apply(multi: MultiService): PathNormalizer = {
    apply(multi.services.flatMap(_.service.resources.flatMap(_.operations)))
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
  private[this] val (staticRouteMap, dynamicRouteMap) = {
    val opsWithRoute = operations.map { op => OperationWithRoute(op) }

    // Map from method name to list of operations
    val tmpDynamicRouteMap = scala.collection.mutable.Map[Method, Seq[OperationWithRoute]]()
    opsWithRoute.foreach { opWithRoute =>
      opWithRoute.route match {
        case r: Route.Dynamic => {
          tmpDynamicRouteMap.get(r.method) match {
            case None => {
              tmpDynamicRouteMap += (r.method -> Seq(opWithRoute))
            }
            case Some(existing) => {
              tmpDynamicRouteMap += (r.method -> (existing ++ Seq(opWithRoute)))
            }
          }
        }
        case _: Route.Static => // no-op
      }
    }

    val staticRoutes = opsWithRoute.flatMap { opWithRoute =>
      opWithRoute.route match {
        case _: Route.Static => Some(opWithRoute)
        case _ => None
      }
    }

    val tmpStaticRouteMap = Map(
      staticRoutes.map { opWithRoute =>
        routeKey(opWithRoute.op.method, opWithRoute.op.path) -> opWithRoute
      }: _*
    )

    (tmpStaticRouteMap, tmpDynamicRouteMap.toMap)
  }


  /**
    * @param method GET, POST, etc.
    * @param path e.g. /users/123
    * @return Either validation errors or the route
    */
  final def resolve(method: String, path: String): Either[Seq[String], Option[Operation]] = {
    Method.fromString(method) match {
      case None => {
        Left(Seq(s"HTTP method '$method' is invalid. Must be one of: " + Method.all.map(_.toString).mkString(", ")))
      }
      case Some(m) => {
        Right(resolve(m, path))
      }
    }
  }

  final def resolve(method: Method, path: String): Option[Operation] = {
    staticRouteMap.get(routeKey(method, path)) match {
      case None => {
        dynamicRouteMap.getOrElse(method, Nil).find { opWithRoute =>
          opWithRoute.route.matches(method, path.trim)
        }.map(_.op)
      }
      case Some(opWithRoute) => {
        Some(opWithRoute.op)
      }
    }
  }

  private[this] def routeKey(method: Method, path: String): String = {
    s"$method:${path.trim}"
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
