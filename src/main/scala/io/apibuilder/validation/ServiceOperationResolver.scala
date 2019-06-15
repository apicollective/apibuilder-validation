package io.apibuilder.validation

import io.apibuilder.spec.v0.models._

private[validation] case class ServiceOperationCache(services: List[ApiBuilderService]) {

  private[this] case class Entry(route: Route, operation: ApiBuilderOperation)

  private[this] val entries: List[Entry] = {
    services.flatMap { s =>
      s.service.resources.flatMap(_.operations).map { op =>
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

  def find(method: Method, path: String): List[ApiBuilderOperation] = {
    entriesByNumberSlashes.getOrElse(numberSlashes(path), List.empty).filter(_.route.matches(method, path)).map(_.operation)
  }
}

case class ServiceOperationResolver(services: List[ApiBuilderService]) {

  private[this] val cache = ServiceOperationCache(services)

  /**
    * resolve the API Builder service defined at the provided method, path.
    * if no service, return a nice error message. Otherwise invoke
    * the provided function on the API Builder service.
    */
  def findOperation(method: Method, path: String): Option[ApiBuilderOperation] = {
    cache.find(method, path) match {
      case Nil => None
      case one :: Nil => Some(one)
      case multiple => {
        // If we find a static path in any service, return that one.
        // Otherwise return the first matching service. This handles ambiguity:
        //   - service 1 defines POST /:organization/tokens
        //   - service 2 defines POST /users/tokens
        // We want to return service 2 when the path is /users/tokens
        multiple.find { apiBuilderOperation =>
          Route.isStatic(apiBuilderOperation.operation.path)
        }.orElse {
          multiple.headOption
        }
      }
    }
  }
}


