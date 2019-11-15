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
    val finalJs = typ match {
      case m: ApiBuilderType.Model => {
        js match {
          case j: JsObject => injectModelsWithOptionalFields(m, j)
          case _ => js
        }
      }
      case _: ApiBuilderType.Enum |  _: ApiBuilderType.Union => js
    }
    validator.validateType(typ, finalJs)
  }


  private[this] def injectModelsWithOptionalFields(typ: ApiBuilderType.Model, js: JsObject): JsObject = {
    typ.requiredFields.filter { f =>
      (js \ f.name).toOption.isEmpty
    }.foldLeft(js) { case (js, field) =>
      if (isFieldModelWithAllRequiredFields(typ.service, field.`type`)) {
        js ++ Json.obj(field.name -> Json.obj())
      } else {
        js
      }
    }
  }

  private[this] def isFieldModelWithAllRequiredFields(service: models.Service, typ: String): Boolean = {
    findType(service.namespace, typ) match {
      case None => false
      case Some(t) => t match {
        case m: ApiBuilderType.Model => !m.model.fields.exists(_.required)
        case _: ApiBuilderType.Enum | _: ApiBuilderType.Union => false
      }
    }
  }
}
