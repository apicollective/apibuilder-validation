package io.apibuilder.validation

import io.apibuilder.spec.v0.models._
import io.apibuilder.validation.util.FileOrder
import io.apibuilder.validation.zip.ZipFileReader
import play.api.libs.json._

/**
  * Wrapper to work with multiple API Builder services.
  * Takes an ordered list of services. If multiple
  * services define an http path, first one is selected.
  */
trait MultiService extends ResponseHelpers {

  def services(): List[ApiBuilderService]

  def findService(method: Method, path: String): Option[ApiBuilderService]

  /**
    * Resolves the type specified
    */
  def findType(typeName: TypeName): Option[ApiBuilderType]

  /**
    * Upcast the json value based on the specified type name, if it is defined. a No-op if not
    */
  def upcast(typ: ApiBuilderType, js: JsValue): Either[Seq[String], JsValue]

  /**
    * @param defaultNamespace e.g. io.flow.user.v0 - used unless the type name
    *                         is fully qualified already
    * @param typeName e.g. 'user' - looks up the API Builder type with this name
    *                 and if found, uses that type to validate and upcast the
    *                 JSON. Note if the type is not found, the JSON returned
    *                 is unchanged.
    */
  final def findType(defaultNamespace: String, typeName: String): Option[ApiBuilderType] = {
    findType(TypeName.parse(name = typeName, defaultNamespace = defaultNamespace))
  }

  final def findType(fullyQualifiedName: String): Option[ApiBuilderType] = {
    TypeName.parse(fullyQualifiedName).flatMap(findType)
  }

  /**
    * If the specified method & path requires a body, returns the type of the body
    */
  final def bodyTypeFromPath(method: String, path: String): Option[ApiBuilderType] = {
    bodyTypeFromPath(Method(method), path)
  }

  final def bodyTypeFromPath(method: Method, path: String): Option[ApiBuilderType] = {
    operation(method, path).flatMap(findBodyType)
  }

  /**
    * For the given method & path, returns the defined operation, if any
    */
  final def operation(method: String, path: String): Option[ApiBuilderOperation] = {
    operation(Method(method), path)
  }

  final def operation(method: Method, path: String): Option[ApiBuilderOperation] = {
    validateOperation(method, path) match {
      case Left(_) => None
      case Right(op) => Some(op)
    }
  }

  /**
    * Validates the js value across all services, upcasting types to
    * match the request method/path as needed.
    */
  final def upcast(apiBuilderOperation: ApiBuilderOperation, js: JsValue): Either[Seq[String], JsValue] = {
    findBodyType(apiBuilderOperation) match {
      case None => Right(js)
      case Some(bodyType) => upcast(bodyType, js)
    }
  }

  final def findBodyType(apiBuilderOperation: ApiBuilderOperation): Option[ApiBuilderType] = {
    apiBuilderOperation.operation.body.flatMap { body =>
      findType(
        defaultNamespace = apiBuilderOperation.service.namespace,
        typeName = body.`type`
      )
    }
  }

  final def upcastOperationBody(method: String, path: String, js: JsValue): Either[Seq[String], JsValue] = {
    upcastOperationBody(Method(method), path, js)
  }

  final def upcastOperationBody(method: Method, path: String, js: JsValue): Either[Seq[String], JsValue] = {
    validateOperation(method, path) match {
      case Left(errors) => Left(errors)
      case Right(op) => upcast(op, js)
    }
  }

  final def upcastType(defaultNamespace: String, typeName: String, js: JsValue): Either[Seq[String], JsValue] = {
    findType(defaultNamespace = defaultNamespace, typeName = typeName) match {
      case None => Right(js)
      case Some(t) => upcast(t, js)
    }
  }

  final def validateOperation(method: String, path: String): Either[Seq[String], ApiBuilderOperation] = {
    validateOperation(Method(method), path)
  }

  /**
    * Validates that the path is known and the method is supported for the path.
    * If known, returns the corresponding operation. Otherwise returns a
    * list of errors.
    */
  final def validateOperation(method: Method, path: String): Either[Seq[String], ApiBuilderOperation] = {
    findService(method, path) match {
      case Some(service) => {
        service.validate(method, path).right.map { op => ApiBuilderOperation(service.service, op) }
      }
      case None => {
        method match {
          case Method.UNDEFINED(name) => {
            Left(Seq(s"HTTP method '$name' is invalid. Must be one of: " + Method.all.map(_.toString).mkString(", ")))
          }
          case _ => {
            val availableMethods = Method.all.filterNot(_ == method).filter { m =>
              findService(m, path).isDefined
            }
            if (availableMethods.isEmpty) {
              Left(Seq(s"HTTP path '$path' is not defined"))
            } else {
              Left(Seq(s"HTTP method '$method' not defined for path '$path' - Available methods: ${availableMethods.map(_.toString).mkString(", ")}"))
            }
          }
        }
      }
    }
  }

  final def allEnums(): Seq[ApiBuilderType.Enum] = {
    services.map(_.service).flatMap { s =>
      s.enums.map { m => ApiBuilderType.Enum(s, m) }
    }
  }

  final def allModels(): Seq[ApiBuilderType.Model] = {
    services.map(_.service).flatMap { s =>
      s.models.map { m => ApiBuilderType.Model(s, m) }
    }
  }

  final def allUnions(): Seq[ApiBuilderType.Union] = {
    services.map(_.service).flatMap { s =>
      s.unions.map { m => ApiBuilderType.Union(s, m) }
    }
  }
}

object MultiService {

  // If this file is found in the zip file, we read files in
  // the order in which they are listed.
  private[this] val OrderByFileName: String = "order.txt"

  def apply(services: List[ApiBuilderService]): MultiService = {
    MultiServiceImpl(services)
  }

  def fromUrl(url: String): Either[Seq[String], MultiService] = {
    fromUrls(urls = Seq(url))
  }

  /**
  * Loads the list of API Builder service specification from the specified URIs,
  * returning either a list of errors or an instance of MultiService
  */
  def fromUrls(urls: Seq[String]): Either[Seq[String], MultiService] = {
    val eithers = urls.flatMap { url =>
      if (ZipFileReader.isZipFile(url)) {
        ZipFileReader.fromUrl(url) match {
          case Left(errors) => Seq(Left(errors))
          case Right(reader) => {
            val fileSorter = FileOrder(reader.entries.find(_.name.toLowerCase() == OrderByFileName).map(_.file))
            reader.entries
              .filter { e => ZipFileReader.isJsonFile(e.name) }
              .sortBy { e => fileSorter.sortOrder(e.name) }
              .map { e => ApiBuilderService.fromFile(e.file) }
          }
        }
      } else {
        Seq(ApiBuilderService.fromUrl(url))
      }
    }

    eithers.flatMap(_.left.getOrElse(Nil)).toList match {
      case Nil => {
        Right(
          MultiServiceImpl(
            services = eithers.map(_.right.get).toList
          )
        )
      }
      case errors => Left(errors)
    }
  }

}
