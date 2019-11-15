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
      case (m: ApiBuilderType.Model, j: JsObject) => injectModelsWithOptionalFields(m, j)
      case _ => js
    }
    validator.validateType(typ, finalJs)
  }

  private[this] def injectModelsWithOptionalFields(typ: ApiBuilderType.Model, incoming: JsObject): JsObject = {
    println(s"injectModelsWithOptionalFields ${typ.model.name}: ${incoming}")
    typ.requiredFields.filter { f =>
      (incoming \ f.name).toOption.isEmpty
    }.foldLeft(incoming) { case (js, field) =>
      println(s"field: ${field.name}")
      createDefault(typ.service, field.`type`) match {
        case None => js
        case Some(defaultJs) => js ++ Json.obj(field.name -> defaultJs)
      }
    }
  }

  private[this] def createDefault(service: models.Service, typ: String): Option[JsObject] = {
    findType(service.namespace, typ).flatMap {
      case m: ApiBuilderType.Model => createDefault(m)
      case _: ApiBuilderType.Enum | _: ApiBuilderType.Union => None
    }
  }

  private[this] def createDefault(typ: ApiBuilderType.Model): Option[JsObject] = {
    val all = typ.requiredFields.map { f =>
      createDefault(typ.service, f.`type`).map { d => Json.obj(f.name -> d) }
    }
    if (all.exists(_.isEmpty)) {
      None
    } else {
      Some(
        all.flatten.foldLeft(Json.obj()) { case (a, b) => a ++ b }
      )
    }
  }
}
