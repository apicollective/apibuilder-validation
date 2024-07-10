package io.apibuilder.validation

import io.apibuilder.spec.v0.models.Method

case class StaticRouteMap(routes: Seq[OperationWithRoute]) {
  private def key(method: Method, path: String): String = {
    s"$method$path"
  }

  private val lookup: Map[String, OperationWithRoute] = Map(
    routes.map { op =>
      key(op.route.method, op.route.path) -> op
    }*
  )

  def find(method: Method, path: String): Option[OperationWithRoute] = {
    lookup.get(key(method, path))
  }
}

case class DynamicRouteMap(routes: Seq[OperationWithRoute]) {

  private val lookup: Map[String, Seq[OperationWithRoute]] = routes.groupBy { r =>
    key(r.route.method, r.route.path)
  }

  private def numberSlashes(path: String): Int = path.count(_ == '/')

  private def key(method: Method, path: String): String = {
    s"$method:${numberSlashes(path)}"
  }

  // TODO: can we make constant time?
  def find(method: Method, path: String): Option[OperationWithRoute] = {
    lookup.get(key(method, path)) match {
      case None => None
      case Some(all) => {
        all.find { opWithRoute =>
          opWithRoute.route.matches(method, path.trim)
        }
      }
    }
  }
}
