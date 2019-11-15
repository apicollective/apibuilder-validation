package io.apibuilder.validation

import io.apibuilder.spec.v0.models
import io.apibuilder.spec.v0.models.Method
import play.api.libs.json.{JsObject, JsValue, Json}

/**
  * Wrapper to work with multiple API Builder services.
  * Takes an ordered list of services. If multiple
  * services define an http path, first one is selected.
  */
case class MultiServiceImpl(
  override val services: List[ApiBuilderService]
) extends MultiService {

  private[this] val validator = JsonValidator(services.map(_.service))
  private[this] val serviceResolver = ServiceOperationResolver(services)

  override def findOperation(method: Method, path: String): Option[ApiBuilderOperation] = {
    serviceResolver.findOperation(method, path)
  }

  override def findType(typ: TypeName): Option[ApiBuilderType] = {
    validator.findType(typ).headOption
  }

  override def upcast(typ: ApiBuilderType, js: JsValue): Either[Seq[String], JsValue] = {
    val finalJs = (typ, js) match {
      case (m: ApiBuilderType.Model, j: JsObject) => {
        val updated = createDefault(m).deepMerge(j)
        if (js != updated && validator.validateType(m, updated).isRight) {
          // Note we only merge here if the merged object itself is valid.
          // This is important as it ensures that any validation messages
          // are clear (highest node) and that we don't inject a default
          // for a nested optional object that does not validate
          updated
        } else {
          js
        }
      }
      case _ => js
    }
    validator.validateType(typ, finalJs)
  }

  /**
   * Creates a json object representing the default values
   * for any models with optional fields.
   */
  private[this] def createDefault(typ: ApiBuilderType.Model): JsObject = {
    typ.model.fields.foldLeft(Json.obj()) { case (js, f) =>
      createDefault(typ.service, f.`type`) match {
        case None => js
        case Some(defaults) => js ++ Json.obj(f.name -> defaults)
      }
    }
  }

  private[this] def createDefault(service: models.Service, typ: String): Option[JsObject] = {
    findType(service.namespace, typ).flatMap {
      case m: ApiBuilderType.Model => Some(createDefault(m))
      case _: ApiBuilderType.Enum | _: ApiBuilderType.Union => None
    }
  }
}
