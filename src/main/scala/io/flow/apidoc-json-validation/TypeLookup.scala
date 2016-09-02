package io.flow.lib.apidoc.json.validation

import com.bryzek.apidoc.spec.v0.models.Service
import play.api.libs.json._

/**
  * Given an http method and a path, resolves the form body type if
  * any associated with that path.
  *
  * Ex:
  * 
  *  TypeLookup(service).forPath("POST", "/users")
  * 
  * => Some("user_form")
  *
  * This class is thread safe
  */
case class TypeLookup(service: Service) {

  private[this] val cache = Map[String, String](
    service.resources.flatMap(_.operations).filter { _.body.isDefined }.map { op =>
      buildKey(op.method.toString, op.path) -> op.body.get.`type`
    }: _*
  )

  def forPath(method: String, path: String): Option[String] = {
    cache.get(buildKey(method, path))
  }

  private[this] def buildKey(method: String, path: String): String = {
    method.toUpperCase + " " + path
  }

}
