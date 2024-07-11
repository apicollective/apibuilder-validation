package io.apibuilder.builders

import java.util.UUID

import io.apibuilder.spec.v0.models.{Annotation, Apidoc, Application, Attribute, Body, Contact, Deprecation, Enum, EnumValue, Field, Header, Import, Info, License, Method, Model, Operation, Organization, Parameter, ParameterLocation, Resource, Response, ResponseCode, ResponseCodeInt, Service, Union, UnionType}
import io.apibuilder.validation.{ApiBuilderType, MultiService}
import play.api.libs.json.{JsObject, Json}

trait ApiBuilderServiceBuilders {

  protected def random(): String = {
    // Start with a letter to ensure name is compatible with api builder validation
    "a" + UUID.randomUUID().toString
  }

  def makeService(
    apidoc: Option[Apidoc] = None,
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
  ): Model = {
    Model(
      name = name,
      plural = plural,
      fields = fields
    )
  }

  def makeField(
    name: String = random().toLowerCase(),
    `type`: String = "string",
    required: Boolean = true,
    default: Option[String] = None,
  ): Field = {
    Field(
      name = name,
      `type` = `type`,
      required = required,
      default = default,
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

  def makeValidEnum(name: String = random()): Enum = {
    makeEnum(
      name = name,
      values = Seq(makeEnumValue()),
    )
  }

  def makeEnum(
    name: String = random(),
    plural: String = random(),
    description: Option[String] = None,
    deprecation: Option[Deprecation] = None,
    values: Seq[EnumValue] = Nil,
    attributes: Seq[Attribute] = Nil,
  ): Enum = {
    Enum(
      name = name,
      plural = plural,
      description = description,
      deprecation = deprecation,
      values = values,
      attributes = attributes,
    )
  }

  def makeEnumValue(
    name: String = random(),
    description: Option[String] = None,
    deprecation: Option[Deprecation] = None,
    attributes: Seq[Attribute] = Nil,
    value: Option[String] = None,
  ): EnumValue = {
    EnumValue(
      name = name,
      description = description,
      deprecation = deprecation,
      attributes = attributes,
      value = value,
    )
  }

  def makeInfo(
    license: Option[License] = None,
    contact: Option[Contact] = None,
  ): Info = {
    Info(license = license, contact = contact)
  }

  def makeUnion(
    name: String = random(),
    plural: String = random(),
    discriminator: Option[String] = None,
    description: Option[String] = None,
    deprecation: Option[Deprecation] = None,
    types: Seq[UnionType] = Nil,
    attributes: Seq[Attribute] = Nil,
    interfaces: Seq[String] = Nil,
  ): Union = {
    Union(
      name = name,
      plural = plural,
      discriminator = discriminator,
      description = description,
      deprecation = deprecation,
      types = types,
      attributes = attributes,
      interfaces = interfaces,
    )
  }

  def makeUnionType(
    `type`: String,
    description: Option[String] = None,
    deprecation: Option[Deprecation] = None,
    attributes: Seq[Attribute] = Nil,
    default: Option[Boolean] = None,
    discriminatorValue: Option[String] = None,
  ): UnionType = {
    UnionType(
      `type` = `type`,
      description = description,
      deprecation = deprecation,
      attributes = attributes,
      default = default,
      discriminatorValue = discriminatorValue,
    )
  }

  def mustFindModel(multi: MultiService, namespace: String, name: String): ApiBuilderType.Model = {
    val t = multi.findType(defaultNamespace = namespace, name).getOrElse {
      sys.error(s"Cannot find namespace[$namespace] name[$name]")
    }
    t match {
      case m: ApiBuilderType.Model => m
      case _ => sys.error(s"Type '$name' is not a model")
    }
  }

  def makeResource(
    `type`: String = random(),
    plural: String = random(),
    path: Option[String] = None,
    description: Option[String] = None,
    deprecation: Option[Deprecation] = None,
    operations: Seq[Operation] = Nil,
    attributes: Seq[Attribute] = Nil,
  ): Resource = {
    Resource(
      `type` = `type`,
      plural = plural,
      path = path,
      description = description,
      deprecation = deprecation,
      operations = operations,
      attributes = attributes,
    )
  }

  def makeAttribute(
    name: String = random(),
    value: JsObject = Json.obj(),
    description: Option[String] = None,
    deprecation: Option[Deprecation] = None,
  ): Attribute = {
    Attribute(
      name = name,
      value = value,
      description = description,
      deprecation = deprecation,
    )
  }

  def makeBody(
    `type`: String = random(),
    description: Option[String] = None,
    deprecation: Option[Deprecation] = None,
    attributes: Seq[Attribute] = Nil,
  ): Body = {
    Body(
      `type` = `type`,
      description = description,
      deprecation = deprecation,
      attributes = attributes,
    )
  }

  def makeOperation(
    method: Method = Method.Get,
    path: String = random(),
    description: Option[String] = None,
    deprecation: Option[Deprecation] = None,
    body: Option[Body] = None,
    parameters: Seq[Parameter] = Nil,
    responses: Seq[Response] = Nil,
    attributes: Seq[Attribute] = Nil,
  ): Operation = {
    Operation(
      method = method,
      path = path,
      description = description,
      deprecation = deprecation,
      body = body,
      parameters = parameters,
      responses = responses,
      attributes = attributes,
    )
  }

  def make200Response(`type`: String): Response = {
    makeResponse(
      code = ResponseCodeInt(200),
      `type` = `type`,
    )
  }

  def makeResponse(
    code: ResponseCode = ResponseCodeInt(200),
    `type`: String = random(),
    headers: Option[Seq[Header]] = None,
    description: Option[String] = None,
    deprecation: Option[Deprecation] = None,
    attributes: Option[Seq[Attribute]] = None,
  ): Response = {
    Response(
      code = code,
      `type` = `type`,
      headers = headers,
      description = description,
      deprecation = deprecation,
      attributes = attributes,
    )
  }

  def makeParameter(
    name: String = random(),
    `type`: String = random(),
    location: ParameterLocation = ParameterLocation.Query,
    description: Option[String] = None,
    deprecation: Option[Deprecation] = None,
    required: Boolean = true,
    default: Option[String] = None,
    minimum: Option[Long] = None,
    maximum: Option[Long] = None,
    example: Option[String] = None,
    attributes: Option[Seq[Attribute]] = None,
  ): Parameter = {
    Parameter(
      name = name,
      `type` = `type`,
      location = location,
      description = description,
      deprecation = deprecation,
      required = required,
      default = default,
      minimum = minimum,
      maximum = maximum,
      example = example,
      attributes = attributes,
    )
  }

  def makeImport(service: Service): Import = {
    Import(
      uri = random(),
      namespace = service.namespace,
      organization = service.organization,
      application = service.application,
      version = service.version,
      enums = service.enums.map(_.name),
      interfaces = service.interfaces.map(_.name),
      unions = service.unions.map(_.name),
      models = service.models.map(_.name),
      annotations = service.annotations,
    )
  }
}
