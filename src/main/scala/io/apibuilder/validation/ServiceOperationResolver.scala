package io.apibuilder.validation

import io.apibuilder.spec.v0.models._

/**
  * A cache of all of the operations defined in the list of services. Allows
  * for resolution from a Method and Path to the service in which that operation
  * is defined.
  *
  * As paths can be dynamic, it's difficult to precache the exact resolution
  * of a path to a service. This cache internally optimized by splitting the
  * path on '/' - and using the number of parts to select only the subset
  * of operations with the same number of parts. We then iterate through
  * this subset to select the specific operation.
  */
private[validation] case class ServiceOperationCache(
  services: List[ApiBuilderService]
)(
  acceptPath: String => Boolean
) {
  private case class Entry(route: Route, operation: ApiBuilderOperation)

  private val entries: List[Entry] = {
    services.flatMap { s =>
      s.service.resources.flatMap(_.operations)
        .filter { op => acceptPath(op.path) }
        .map { op =>
        Entry(
          Route(op.method, op.path),
          ApiBuilderOperation(s, op)
        )
      }
    }
  }

  private val entriesByNumberSlashes: Map[Int, List[Entry]] = entries.groupBy { e =>
    numberSlashes(e.route.path)
  }

  private def numberSlashes(path: String): Int = path.count(_ == '/')

  def findOperation(method: Method, path: String): Option[ApiBuilderOperation] = {
    entriesByNumberSlashes.getOrElse(numberSlashes(path), List.empty)
      .find(_.route.matches(method, path))
      .map(_.operation)
  }
}

case class ServiceOperationResolver(services: List[ApiBuilderService]) {

  private val static = ServiceOperationCache(services)(Route.isStatic)
  private val dynamic = ServiceOperationCache(services)(Route.isDynamic)

  // If we find a static path in any service, return that one.
  // Otherwise return the first matching service. This handles ambiguity:
  //   - service 1 defines POST /:organization/tokens
  //   - service 2 defines POST /users/tokens
  // We want to return service 2 when the path is /users/tokens
  def findOperation(method: Method, path: String): Option[ApiBuilderOperation] = {
    static.findOperation(method, path).orElse {
      dynamic.findOperation(method, path)
    }
  }

}


