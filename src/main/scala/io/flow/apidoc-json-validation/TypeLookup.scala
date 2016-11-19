package io.flow.lib.apidoc.json.validation

import com.bryzek.apidoc.spec.v0.models.{Method, Operation, Service}
import play.api.libs.json._

/**
  * Given an http method and a path, resolves the form body type if
  * any associated with that path.
  *
  * Ex:
  * 
  *  TypeLookup(service).path("/users").method("POST")
  * 
  * => Some("user_form")
  *
  * This class is thread safe
  **/
case class TypeLookup(service: Service) {

  private[this] val byPaths: Map[String, Map[Method, Operation]] = {
    val tmp = scala.collection.mutable.Map[String, scala.collection.mutable.Map[Method, Operation]]()
    service.resources.flatMap(_.operations).map { op =>
      val m = tmp.get(op.path).getOrElse {
        val map = scala.collection.mutable.Map[Method, Operation]()
        tmp += op.path -> map
        map
      }
      m += op.method -> op
    }
    tmp.map {
      case (path, methods) => (path -> methods.toMap)
    }.toMap
  }

  /**
    * If the specified method, path require a body, returns the type of the body
    */
  def bodyTypeFromPath(method: String, path: String): Option[String] = {
    operationsByMethod(path).get(Method(method)).flatMap(_.body.map(_.`type`))
  }

  /**
    * Returns a map of the operations available for the specified path. Keys are the HTTP Methods.
    */
  def operationsByMethod(path: String): Map[Method, Operation] = {
    byPaths.get(path).getOrElse(Map.empty)
  }

}
