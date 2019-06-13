package io.apibuilder.validation

import java.util.concurrent.ConcurrentHashMap

import io.apibuilder.spec.v0.models._
import play.api.libs.json._

/**
  * Wrapper to work with multiple API Builder services.
  * Takes an ordered list of services. If multiple
  * services define an http path, first one is selected.
  */
case class MultiServiceImpl(
  services: Seq[ApiBuilderService]
) extends MultiService {

  private[this] val validator = JsonValidator(services.map(_.service))

  def findType(name: String): Seq[ApibuilderType] = validator.findType(name, defaultNamespace = None)

  def findType(namespace: String, name: String): Seq[ApibuilderType] = validator.findType(namespace, name)

  def upcast(typeName: String, js: JsValue): Either[Seq[String], JsValue] = {
    validator.validate(typeName, js, defaultNamespace = None)
  }

  def upcast(typ: ApibuilderType, js: JsValue): Either[Seq[String], JsValue] = {
    validator.validateType(typ, js)
  }

  def validate(method: Method, path: String): Either[Seq[String], Operation] = {
    resolveService(method, path) match {
      case Left(errors) => Left(errors)
      case Right(service) => service.validate(method, path)
    }
  }

  def validate(
    typ: ApibuilderType,
    js: JsValue,
    prefix: Option[String] = None
  ): Either[Seq[String], JsValue] = {
    validator.validateType(typ, js, prefix)
  }

  private[this] val resolveServiceCache = new ConcurrentHashMap[String, Either[Seq[String], ApiBuilderService]]()

  /**
    * resolve the API Builder service defined at the provided method, path.
    * if no service, return a nice error message. Otherwise invoke
    * the provided function on the API Builder service.
    */
  def resolveService(method: Method, path: String): Either[Seq[String], ApiBuilderService] = {
    resolveServiceCache.computeIfAbsent(
      s"$method$path",
      _ => { doResolveService(method, path) }
    )
  }

  private[this] def doResolveService(method: Method, path: String): Either[Seq[String], ApiBuilderService] = {
    services.filter { s =>
      s.isDefinedAt(method = method, path = path)
    } match {
      case Nil => {
        services.find(_.isPathDefinedAt(path)) match {
          case None => {
            Left(Seq(s"HTTP path '$path' is not defined"))
          }

          case Some(s) => s.validate(method, path) match {
            case Left(errors) => Left(errors)
            case Right(_) => Right(s)
          }
        }
      }
      case one :: Nil => Right(one)

      case multiple => {
        // If we find a non dynamic path in any service, return that one.
        // Otherwise return the first matching service. This handles ambiguity:
        //   - service 1 defines POST /:organization/tokens
        //   - service 2 defines POST /users/tokens
        // We want to return service 2 when the path is /users/tokens
        Right(
          multiple.find { s =>
            s.validate(method, path) match {
              case Right(op) if Route.isStatic(op.path) => true
              case _ => false
            }
          }.getOrElse {
            multiple.head
          }
        )
      }
    }
  }

}


