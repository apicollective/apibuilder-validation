package io.apibuilder.validation.helpers

import io.apibuilder.spec.v0.models._
import java.util.UUID

import io.apibuilder.validation.{ApibuilderType, MultiService}

trait ApiBuilderServiceHelpers {
  
  private[this] def random(): String = {
    UUID.randomUUID().toString
  }

  def makeService(
    apidoc: Apidoc = makeApidoc(),
    name: String = random(),
    organization: Organization = makeOrganization(),
    application: Application = makeApplication(),
    namespace: String = s"test.apicollective.${random()}",
    version: String = "1.2.3",
    baseUrl: Option[String] = None,
    description: Option[String] = None,
    info: Info = makeInfo(),
    headers: Seq[Header] = Nil,
    imports: Seq[Import] = Nil,
    enums: Seq[Enum] = Nil,
    unions: Seq[Union] = Nil,
    models: Seq[Model] = Nil,
    resources: Seq[Resource] = Nil,
    attributes: Seq[Attribute] = Nil,
    annotations: Seq[Annotation] = Nil
  ): Service = {
    Service(
      apidoc = apidoc,
      name = name,
      organization = organization,
      application = application,
      namespace = namespace,
      version = version,
      baseUrl = baseUrl,
      description = description,
      info = info,
      headers = headers,
      imports = imports,
      enums = enums,
      unions = unions,
      models = models,
      resources = resources,
      attributes = attributes,
      annotations = annotations
    )
  }
  
  def makeModel(
    name: String = random(),
    plural: String = random(),
    fields: Seq[Field] = Nil,
    namespace: String = "test.apicollective",
  ): Model = {
    Model(
      name = name,
      plural = name + "s",
      fields = fields
    )
  }

  def makeField(
    name: String = random().toLowerCase(),
    `type`: String = "string",
    required: Boolean = true
  ): Field = {
    Field(
      name = name,
      `type` = `type`,
      required = required
    )
  }

  def makeApidoc(version: String = "1.0.0"): Apidoc = {
    Apidoc(version = version)
  }

  def makeOrganization(key: String = "apicollective"): Organization = {
    Organization(key = key)
  }

  def makeApplication(key: String = "tests"): Application = {
    Application(key = key)
  }

  def makeInfo(
    license: Option[License] = None,
    contact: Option[Contact] = None
  ): Info = {
    Info(license = license, contact = contact)
  }

  def mustFindModel(multi: MultiService, name: String): ApibuilderType.Model = {
    multi.findType(name).toList match {
      case Nil => sys.error(s"Cannot find type $name")
      case one :: Nil => {
        one match {
          case m: ApibuilderType.Model => m
          case other => sys.error(s"Type '$name' is not a model")
        }
      }
      case multiple => sys.error(s"Cannot find type $name - multiple matches: $multiple")
    }
  }

}

