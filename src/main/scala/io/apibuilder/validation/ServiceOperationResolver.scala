package io.apibuilder.validation

import io.apibuilder.spec.v0.models._

case class ServiceOperationResolver(services: List[ApiBuilderService]) {

  /**
    * resolve the API Builder service defined at the provided method, path.
    * if no service, return a nice error message. Otherwise invoke
    * the provided function on the API Builder service.
    */
  def findOperation(method: Method, path: String): Option[ApiBuilderOperation] = {
    services.flatMap { s =>
      s.findOperation(method, path).map { op =>
        ApiBuilderOperation(s, op)
      }
    } match {
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


