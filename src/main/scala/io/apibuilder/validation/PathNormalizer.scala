package io.apibuilder.validation

import io.apibuilder.spec.v0.models.{Method, Operation}

/**
  * Normalizes paths based on variables defined in the path.
  * For example, a path like "/users/123" would resolve to "/users/:id"
  * assuming there is a valid operation for that path.
  *
  * Stategy:
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

    // Map from method name to list of operations
    val tmpDynamicRouteMap = scala.collection.mutable.Map[Method, Seq[Operation]]()
    operations.foreach { op =>
      Route(op) match {
        case r: Route.Dynamic => {
          tmpDynamicRouteMap.get(r.method) match {
            case None => {
              tmpDynamicRouteMap += (r.method -> Seq(op))
            }
            case Some(existing) => {
              tmpDynamicRouteMap += (r.method -> (existing ++ Seq(op)))
            }
          }
        }
        case _: Route.Static => // no-op
      }
    }

    val staticRoutes = operations.flatMap { op =>
      Route(op) match {
        case _: Route.Dynamic => None
        case _: Route.Static => Some(op)
      }
    }

    val tmpStaticRouteMap = Map(
      staticRoutes.map { op =>
        routeKey(op.method, op.path) -> op
      }: _*
    )

    (tmpStaticRouteMap, tmpDynamicRouteMap.toMap)
  }

  final def resolve(method: Method, path: String): Option[Operation] = {
    staticRouteMap.get(routeKey(method, path)) match {
      case None => {
        dynamicRouteMap.getOrElse(method, Nil).find{ op =>
          Route(op).matches(method, path.trim)
        }
      }
      case Some(op) => {
        Some(op)
      }
    }
  }

  private[this] def routeKey(method: Method, path: String): String = {
    s"$method:${path.trim}"
  }
}
