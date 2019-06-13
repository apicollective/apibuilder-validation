package io.apibuilder.validation

import io.apibuilder.spec.v0.models.Method

case class StaticRouteMap(routes: Seq[OperationWithRoute]) {
  private[this] def key(method: Method, path: String): String = {
    s"$method$path"
  }

  private[this] val lookup: Map[String, OperationWithRoute] = Map(
    routes.map { op =>
      key(op.route.method, op.route.path) -> op
    }: _*
  )

  def find(method: Method, path: String): Option[OperationWithRoute] = {
    lookup.get(key(method, path))
  }
}

case class DynamicRouteMap(routes: Seq[OperationWithRoute]) {

  private[this] val byMethod: Map[Method, Seq[OperationWithRoute]] = routes.groupBy(_.route.method)

  // TODO: can we make constant time?
  def find(method: Method, path: String): Option[OperationWithRoute] = {
    byMethod.get(method) match {
      case None => None
      case Some(all) => {
        all.find { opWithRoute =>
          opWithRoute.route.matches(method, path.trim)
        }
      }
    }
  }
}



