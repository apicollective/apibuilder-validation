package io.apibuilder.validation

import io.apibuilder.spec.v0.models.{Method, Operation}

sealed trait Route {

  def method: Method

  def path: String

  def matches(incomingMethod: Method, incomingPath: String): Boolean
}

object Route {

  /**
    * Represents a static route (e.g. /users) with no variables
    */
  case class Static(method: Method, path: String) extends Route {

    override def matches(incomingMethod: Method, incomingPath: String): Boolean = {
      method == incomingMethod && path == incomingPath
    }

  }

  /**
    * Represents a dynamic route (e.g. /users/:id) with
    * wildcards. We implement this be building a regular expression
    * that replaces any ":xxx" with a pattern of one or more
    * characters that are not a '/'
    */
  case class Dynamic(method: Method, path: String) extends Route {

    private[this] val pattern = (
      "^" +
        path.split("/").map { p =>
          if (p.startsWith(":")) {
            """[^\/]+"""
          } else {
            p
          }
        }.mkString("""\/""") +
        "$"
      ).r

    override def matches(incomingMethod: Method, incomingPath: String): Boolean = {
      if (method == incomingMethod) {
        incomingPath match {
          case pattern() => true
          case _ => false
        }
      } else {
        false
      }
    }

  }

  def apply(op: Operation): Route = {
    apply(op.method, op.path)
  }

  def apply(method: Method, path: String): Route = {
    if (path.indexOf(":") >= 0) {
      Dynamic(method = method, path = path)
    } else {
      Static(method = method, path = path)
    }
  }

}
