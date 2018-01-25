package io.apibuilder.validation

import io.apibuilder.spec.v0.models

sealed trait ApibuilderType {
  def nameSpace: String
}

object ApibuilderType {
  case class Enum(nameSpace: String, enum: models.Enum) extends ApibuilderType
  case class Model(nameSpace: String, model: models.Model) extends ApibuilderType
  case class Union(nameSpace: String, union: models.Union) extends ApibuilderType
}
