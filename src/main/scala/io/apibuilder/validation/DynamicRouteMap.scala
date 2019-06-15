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

case class OperationWithRouteSet(routes: Seq[OperationWithRoute]) {

  private[this] val byNumberSlashes: Map[Int, Seq[OperationWithRoute]] = routes.groupBy { r =>
    numberSlashes(r.route.path)
  }

  private[this] def numberSlashes(path: String): Int = 0//path.count(_ == '/')

  def find(method: Method, path: String): Option[OperationWithRoute] = {
    byNumberSlashes.get(numberSlashes(path)) match {
      case None => None
      case Some(candidateRoutes) => {
        candidateRoutes.find { opWithRoute =>
          opWithRoute.route.matches(method, path.trim)
        }
      }
    }
  }
}

case class DynamicRouteMap(routes: Seq[OperationWithRoute]) {

  private[this] val byMethod: Map[Method, OperationWithRouteSet] = {
    routes.groupBy(_.route.method).map { case (m, ops) =>
        m -> OperationWithRouteSet(ops)
    }
  }

  // TODO: can we make constant time?
  def find(method: Method, path: String): Option[OperationWithRoute] = {
    byMethod.get(method) match {
      case None => None
      case Some(routeSet) => routeSet.find(method, path)
    }
  }
}



