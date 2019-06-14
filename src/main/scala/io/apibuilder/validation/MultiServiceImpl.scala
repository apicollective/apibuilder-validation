package io.apibuilder.validation

import play.api.libs.json.JsValue

/**
  * Wrapper to work with multiple API Builder services.
  * Takes an ordered list of services. If multiple
  * services define an http path, first one is selected.
  */
case class MultiServiceImpl(
  override val services: Seq[ApiBuilderService]
) extends MultiService {

  private[this] val validator = JsonValidator(services.map(_.service))

  override def findType(defaultNamespace: String, typeName: String): Option[ApiBuilderType] = {
    validator.findType(
      name = typeName,
      defaultNamespace = Some(defaultNamespace)
    ).headOption
  }

  override def upcast(typ: ApiBuilderType, js: JsValue): Either[Seq[String], JsValue] = {
    validator.validateType(typ, js)
  }

}
