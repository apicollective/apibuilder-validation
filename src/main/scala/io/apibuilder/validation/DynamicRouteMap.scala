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

  private[this] val byNormalizedPath: Map[String, OperationWithRoute] = Map(
    routes.map { r =>
      r.route.path -> r
    }: _*
  )

  byNormalizedPath.take(10).foreach { case (k,_) => println(k)}

  def find(method: Method, path: String): Option[OperationWithRoute] = {
    None
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



