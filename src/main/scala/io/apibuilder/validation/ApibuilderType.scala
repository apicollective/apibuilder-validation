package io.apibuilder.validation

import io.apibuilder.spec.v0.models

sealed trait ApibuilderType {

  def service: models.Service

  def typeName: TypeName

  def namespace: String = typeName.namespace.getOrElse(service.namespace)

  def name: String = typeName.name

  /**
   * returns the fully qualified name of this type (ie including the namespace)
   */
  def qualified: String = s"$namespace.$typeDiscriminator.$name"

  protected def typeDiscriminator: String
}

object ApibuilderType {
  case class Enum(service: models.Service, enum: models.Enum) extends ApibuilderType {
    override val typeName = TypeName(enum.name)
    override val typeDiscriminator = "enums"
  }
  case class Model(service: models.Service, model: models.Model) extends ApibuilderType {
    override val typeName = TypeName(model.name)
    override val typeDiscriminator = "models"
  }
  case class Union(service: models.Service, union: models.Union) extends ApibuilderType {
    override val typeName = TypeName(union.name)
    override val typeDiscriminator = "unions"
  }
}
