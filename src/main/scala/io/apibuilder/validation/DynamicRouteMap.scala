package io.apibuilder.validation

import io.apibuilder.spec.v0.models.Method

case class RouteKey(method: Method, path: String)

case class StaticRouteMap(data: Map[RouteKey, OperationWithRoute]) {
  def find(method: Method, path: String): Option[OperationWithRoute] = {
    data.get(RouteKey(method, path))
  }
}
case class DynamicRouteMap(data: Map[Method, Seq[OperationWithRoute]]) {
  // make constant time
  def find(method: Method, path: String): Option[OperationWithRoute] = {
    data.getOrElse(method, Nil).find { opWithRoute =>
      opWithRoute.route.matches(method, path.trim)
    }
  }
}



