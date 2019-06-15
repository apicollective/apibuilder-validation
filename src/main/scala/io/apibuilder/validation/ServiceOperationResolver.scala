package io.apibuilder.validation

import io.apibuilder.spec.v0.models._

private[validation] case class ServiceOperationByTypeCache(
  services: List[ApiBuilderService]
)(
  acceptPath: String => Boolean
) {
  private[this] case class Entry(route: Route, operation: ApiBuilderOperation)

  private[this] val entries: List[Entry] = {
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

  private[this] val entriesByNumberSlashes: Map[Int, List[Entry]] = entries.groupBy { e =>
    numberSlashes(e.route.path)
  }

  private[this] def numberSlashes(path: String): Int = path.count(_ == '/')

  def find(method: Method, path: String): Option[ApiBuilderOperation] = {
    entriesByNumberSlashes.getOrElse(numberSlashes(path), List.empty)
      .find(_.route.matches(method, path))
      .map(_.operation)
  }
}

private[validation] case class ServiceOperationCache(services: List[ApiBuilderService]) {

  private[this] val static = ServiceOperationByTypeCache(services)(Route.isStatic)
  private[this] val dynamic = ServiceOperationByTypeCache(services)(Route.isDynamic)

  // If we find a static path in any service, return that one.
  // Otherwise return the first matching service. This handles ambiguity:
  //   - service 1 defines POST /:organization/tokens
  //   - service 2 defines POST /users/tokens
  // We want to return service 2 when the path is /users/tokens
  def find(method: Method, path: String): Option[ApiBuilderOperation] = {
    static.find(method, path).orElse {
      dynamic.find(method, path)
    }
  }
}

case class ServiceOperationResolver(services: List[ApiBuilderService]) {

  private[this] val cache = ServiceOperationCache(services)

  /**
    * resolve the API Builder service & operation defined at the provided
    * method, path.
    */
  def findOperation(method: Method, path: String): Option[ApiBuilderOperation] = {
    cache.find(method, path)
  }
}


