package io.apibuilder.validation

import io.apibuilder.spec.v0.models
import io.apibuilder.spec.v0.models.{Field, UnionType}

sealed trait TypeDiscriminator
object TypeDiscriminator {
  case object Enums extends TypeDiscriminator { override def toString = "enums" }
  case object Models extends TypeDiscriminator { override def toString = "models" }
  case object Unions extends TypeDiscriminator { override def toString = "unions" }
}

sealed trait Scalar extends ApiBuilderType
object Scalar {
  
}

sealed trait ApiBuilderType {

  def service: ApiBuilderService
  final def namespace: String = service.namespace
  def name: String
  def typeDiscriminator: TypeDiscriminator

  /**
   * returns the fully qualified name of this type (ie including the namespace)
   */
  def qualified: String = s"$namespace.$typeDiscriminator.$name"
}

case class ApiBuilderField(model: ApiBuilderType.Model, field: Field)
case class ApiBuilderUnionType(union: ApiBuilderType.Union, `type`: UnionType)

object ApiBuilderType {
  case class Enum(override val service: ApiBuilderService, enum: models.Enum) extends ApiBuilderType {
    override val name: String = enum.name
    override val typeDiscriminator: TypeDiscriminator = TypeDiscriminator.Enums
  }
  case class Model(override val service: ApiBuilderService, model: models.Model) extends ApiBuilderType {
    override val name: String = model.name
    override val typeDiscriminator: TypeDiscriminator = TypeDiscriminator.Models
    val fields: Seq[ApiBuilderField] = model.fields.map { f =>
      ApiBuilderField(this, f)
    }
  }
  case class Union(override val service: ApiBuilderService, union: models.Union) extends ApiBuilderType {
    override val name: String = union.name
    override val typeDiscriminator: TypeDiscriminator = TypeDiscriminator.Unions
    val types: Seq[ApiBuilderUnionType] = union.types.map { t =>
      ApiBuilderUnionType(this, t)
    }
  }
}
