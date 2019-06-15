package io.apibuilder.validation

import io.apibuilder.spec.v0.models._

case class ServiceResolver(services: List[ApiBuilderService]) {

  /**
    * resolve the API Builder service defined at the provided method, path.
    * if no service, return a nice error message. Otherwise invoke
    * the provided function on the API Builder service.
    */
  def resolve(method: Method, path: String): Option[ApiBuilderService] = {
    services.filter { s =>
      s.isDefinedAt(method = method, path = path)
    } match {
      case Nil => None
      case one :: Nil => Some(one)
      case multiple => {
        // If we find a static path in any service, return that one.
        // Otherwise return the first matching service. This handles ambiguity:
        //   - service 1 defines POST /:organization/tokens
        //   - service 2 defines POST /users/tokens
        // We want to return service 2 when the path is /users/tokens
        multiple.find { s =>
          s.validate(method, path) match {
            case Right(op) if Route.isStatic(op.path) => true
            case _ => false
          }
        }.orElse {
          multiple.headOption
        }
      }
    }
  }
}


