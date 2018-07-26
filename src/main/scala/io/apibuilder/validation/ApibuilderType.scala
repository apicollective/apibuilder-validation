package io.apibuilder.validation

import io.apibuilder.spec.v0.models

sealed trait ApibuilderType {
  def service: models.Service
}

object ApibuilderType {
  case class Enum(service: models.Service, enum: models.Enum) extends ApibuilderType
  case class Model(service: models.Service, model: models.Model) extends ApibuilderType
  case class Union(service: models.Service, union: models.Union) extends ApibuilderType
}
