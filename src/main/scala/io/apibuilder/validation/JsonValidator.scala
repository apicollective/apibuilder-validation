package io.apibuilder.validation

import java.util.concurrent.ConcurrentHashMap

import cats.implicits._
import cats.data.ValidatedNec
import io.apibuilder.spec.v0.models.{Service, UnionType}
import org.joda.time.format.ISODateTimeFormat
import play.api.libs.json._

import scala.util.{Failure, Success, Try}

/**
  * We auto convert strings to booleans based on common, well known
  * values from various frameworks. For example, the string 'true' or
  * 't' will result in the boolean true - see TrueValues and
  * FalseValues for all supported strings.
  */
object Booleans {

  val TrueValues: Seq[String] = Seq("t", "true", "y", "yes", "on", "1", "trueclass")
  val FalseValues: Seq[String] = Seq("f", "false", "n", "no", "off", "0", "falseclass")

}

object JsonValidator {
  def apply(service: Service): JsonValidator = new JsonValidator(ValidatedJsonValidator(service))
  def apply(services: List[Service]): JsonValidator = new JsonValidator(ValidatedJsonValidator(services))
}

case class JsonValidator(validator: ValidatedJsonValidator) {
  import io.apibuilder.validation.util.Implicits._

  def findType(name: String, defaultNamespace: Option[String]) =
    validator.findType(name, defaultNamespace)

  def findType(defaultNamespace: String, name: String) =
    validator.findType(defaultNamespace, name)

  def findType(typeName: TypeName): Seq[ApiBuilderType] =
    validator.findType(typeName)

  def validate(typeName: String, js: JsValue, defaultNamespace: Option[String], prefix: Option[String] = None): Either[Seq[String], JsValue] =
    validator.validate(typeName, js, defaultNamespace, prefix).toEither.leftToSeq

  def validateType(typ: ApiBuilderType, js: JsValue, prefix: Option[String] = None): Either[Seq[String], JsValue] =
    validator.validateType(typ, js, prefix).toEither.leftToSeq

  def validateString(prefix: String, js: JsValue): Either[Seq[String], JsString] =
    validator.validateString(prefix, js).toEither.leftToSeq

  def validateArray(prefix: String, internalType: String, js: JsValue, defaultNamespace: Option[String]): Either[Seq[String], JsArray] =
    validator.validateArray(prefix, internalType, js, defaultNamespace).toEither.leftToSeq

  def validateObject(prefix: String, internalType: String, js: JsValue, defaultNamespace: Option[String]): Either[Seq[String], JsObject] =
    validator.validateObject(prefix, internalType, js, defaultNamespace).toEither.leftToSeq

  def validateInteger(prefix: String, js: JsValue): Either[Seq[String], JsNumber] =
    validator.validateInteger(prefix, js).toEither.leftToSeq

  def validateDouble(prefix: String, js: JsValue): Either[Seq[String], JsNumber] =
    validator.validateDouble(prefix, js).toEither.leftToSeq

  def validateDecimal(prefix: String, js: JsValue): Either[Seq[String], JsNumber] =
    validator.validateDecimal(prefix, js).toEither.leftToSeq

  def validateLong(prefix: String, js: JsValue): Either[Seq[String], JsNumber] =
    validator.validateLong(prefix, js).toEither.leftToSeq

  def validateUuid(prefix: String, js: JsValue): Either[Seq[String], JsString] =
    validator.validateUuid(prefix, js).toEither.leftToSeq

  def validateDateIso8601(prefix: String, js: JsValue): Either[Seq[String], JsString] =
    validator.validateDateIso8601(prefix, js).toEither.leftToSeq

  def validateDateTimeIso8601(prefix: String, js: JsValue): Either[Seq[String], JsString] =
    validator.validateDateTimeIso8601(prefix, js).toEither.leftToSeq

  def validateBoolean(prefix: String, js: JsValue): Either[Seq[String], JsBoolean] =
    validator.validateBoolean(prefix, js).toEither.leftToSeq
}

object ValidatedJsonValidator {
  def apply(service: Service): ValidatedJsonValidator = ValidatedJsonValidator(List(service))
}

case class ValidatedJsonValidator(services: List[Service]) {
  assert(services.nonEmpty, s"Must have at least one service")

  def findType(name: String, defaultNamespace: Option[String]): Seq[ApiBuilderType] = {
    defaultNamespace match {
      case None => {
        services.map { service =>
          TypeName.parse(defaultNamespace = service.namespace, name = name)
        }.distinct.flatMap { typeName =>
          findType(defaultNamespace = typeName.namespace, name = typeName.name)
        }
      }
      case Some(ns) => {
        findType(ns, name)
      }
    }
  }

  def findType(defaultNamespace: String, name: String): Seq[ApiBuilderType] = {
    findType(TypeName.parse(defaultNamespace = defaultNamespace, name = name))
  }

  private[this] val cache = new ConcurrentHashMap[TypeName, Seq[ApiBuilderType]]()

  def findType(typeName: TypeName): Seq[ApiBuilderType] = {
    cache.computeIfAbsent(
      typeName,
      _ => {
        services.filter(_.namespace.equalsIgnoreCase(typeName.namespace)).flatMap { service =>
          findType(service, typeName.name)
        }
      }
    )
  }

  private[this] def findType(service: Service, typeName: String): Option[ApiBuilderType] = {
    service.enums.find(_.name.equalsIgnoreCase(typeName)) match {
      case Some(e) => Some(ApiBuilderType.Enum(service, e))
      case None => {
        service.models.find(_.name.equalsIgnoreCase(typeName)) match {
          case Some(m) => Some(ApiBuilderType.Model(service, m))
          case None => {
            service.unions.find(_.name.equalsIgnoreCase(typeName)) match {
              case Some(u) => Some(ApiBuilderType.Union(service, u))
              case None => None
            }
          }
        }
      }
    }
  }

  /**
    * Validates the incoming JsValue against the API Builder schema,
    * returning either human friendly validation errors or a new
    * JsValue with any conversions applied (e.g. strings to booleans,
    * numbers to string, etc. as dictated by the schema).
    */
  def validate(
    typeName: String,
    js: JsValue,
    defaultNamespace: Option[String],
    prefix: Option[String] = None
  ): ValidatedNec[String, JsValue] = {
    findType(typeName, defaultNamespace = defaultNamespace).toList match {
      case Nil => {
        // may be a primitive type like 'string'
        validateApiBuilderType(
          prefix.getOrElse(typeName),
          typeName,
          js,
          defaultNamespace
        )
      }

      case typ :: Nil => {
        validateType(typ, js, prefix)
      }

      case _ => {
        // multiple types matches - insufficient data to validate
        js.validNec
      }
    }
  }

  def validateType(
    typ: ApiBuilderType,
    js: JsValue,
    prefix: Option[String] = None
  ): ValidatedNec[String, JsValue] = {
    typ match {
      case e: ApiBuilderType.Enum => {
        validateEnum(prefix.getOrElse("Body"), e, js)
      }

      case m: ApiBuilderType.Model => {
        toObject(prefix.getOrElse("Body"), js).andThen(validateModel(m, _, prefix))
      }

      case u: ApiBuilderType.Union => {
        toObject(prefix.getOrElse("Body"), js).andThen(validateUnion(u, _, prefix))
      }
    }
  }

  private[this] def toObject(prefix: String, js: JsValue): ValidatedNec[String, JsObject] = {
    js match {
      case _: JsArray => {
        s"$prefix must be an object and not an array".invalidNec
      }
      case _: JsBoolean => s"$prefix must be an object and not a boolean".invalidNec
      case JsNull => s"$prefix must be an object and not null".invalidNec
      case _: JsNumber => s"$prefix must be an object and not a number".invalidNec
      case v: JsObject =>
        // Remove null fields as there is nothing to validate there
        JsObject(
          v.value.filter { case (_, value) =>
              value match {
                case JsNull => false
                case _ => true
              }
          }
        ).validNec
      case _: JsString => s"$prefix must be an object and not a string".invalidNec
    }
  }

  private[this] def validateEnum(
    prefix: String,
    typ: ApiBuilderType.Enum,
    js: JsValue
  ): ValidatedNec[String, JsValue] = {
    validateString(prefix, js).andThen { jsString =>
        val incomingValue = jsString.value.trim
        val valid = typ.enum.values.flatMap { e =>
          Seq(e.name) ++ e.value.toSeq
        }
        valid.find { _.toLowerCase.trim == incomingValue.toLowerCase } match {
          case None =>
            (s"$prefix invalid value '${incomingValue}'. Valid values for the enum '${typ.enum.name}' are: " + valid.distinct.mkString("'", "', '", "'")).invalidNec
          case Some(validValue) =>
            JsString(validValue).validNec
        }
    }
  }

  private[this] def validateModel(
    typ: ApiBuilderType.Model,
    js: JsObject,
    prefix: Option[String]
  ): ValidatedNec[String, JsValue] = {

    val missingFields = typ.requiredFields.filter { f =>
      (js \ f.name).toOption.isEmpty
    }.map(_.name).toList

    val missingFieldsV = missingFields match {
      case Nil => ().validNec[String]
      case one :: Nil => s"Missing required field for ${typ.model.name}: $one".invalidNec[Unit]
      case multiple => (s"Missing required fields for ${typ.model.name}: " + multiple.mkString(", ")).invalidNec[Unit]
    }

    val invalidTypesV: ValidatedNec[String, JsObject] = js.fields.foldLeft(Json.obj().validNec[String]) { case (agg, (name, value)) =>
      typ.model.fields.find(_.name == name) match {
        case None => {
          agg
        }

        case Some(f) => {
          agg.andThen { agg =>
            validate(
              typeName = f.`type`,
              js = value,
              prefix = Some(
                prefix.getOrElse(typ.model.name) + s".${f.name}"
              ),
              defaultNamespace = Some(typ.service.namespace)
            ).map { v =>
              agg ++ Json.obj(name -> v)
            }
          }
        }
      }
    }

    (missingFieldsV, invalidTypesV).mapN { (_, updated) =>
      js ++ updated
    }
  }

  private[this] def validateUnion(
    typ: ApiBuilderType.Union,
    js: JsObject,
    prefix: Option[String]
  ): ValidatedNec[String, JsValue] = {
    typ.union.discriminator match {
      case None => {
        js.validNec
      }

      case Some(discriminator) => {
        val disc = (js \ discriminator).asOpt[String]

        def specificTypeDiscriminator(ut: UnionType): String = {
          ut.discriminatorValue.getOrElse(ut.`type`)
        }

        val unionType = disc match {
          case None => typ.union.types.find(_.default.getOrElse(false))
          case Some(t) => typ.union.types.find { ut => specificTypeDiscriminator(ut) == t }
        }

        unionType match {
          case None => {
            disc match {
              case None => {
                s"Union type '${typ.union.name}' requires a field named '$discriminator'".invalidNec
              }
              case Some(value) => {
                (s"Invalid discriminator '$value' for union type '${typ.union.name}': must be one of " + typ.union.types.map { ut => specificTypeDiscriminator(ut) }.mkString("'", "', '", "'")).invalidNec
              }
            }

          }

          case Some(t) => {
            assert(t.`type` != typ.union.name, s"Specific union type name cannot match name of the union itself - else we'd recurse infinitely")
            validate(t.`type`, js, defaultNamespace = Some(typ.service.namespace), prefix = prefix)
          }
        }
      }
    }
  }

  private[this] val ArrayPattern = """^\[(.+)\]$""".r
  private[this] val ObjectPattern = """^map\[(.+)\]$""".r

  /**
    * Validates the JS Value based on the expected API Builder type.
    */
  private[this] def validateApiBuilderType(
    prefix: String,
    typ: String,
    js: JsValue,
    defaultNamespace: Option[String]
  ): ValidatedNec[String, JsValue] = {
    typ.trim.toLowerCase match {
      case "string" => validateString(prefix, js)
      case "integer" => validateInteger(prefix, js)
      case "long" => validateLong(prefix, js)
      case "boolean" => validateBoolean(prefix, js)
      case "double" => validateDouble(prefix, js)
      case "decimal" => validateDecimal(prefix, js)
      case "uuid" => validateUuid(prefix, js)
      case "date-iso8601" => validateDateIso8601(prefix, js)
      case "date-time-iso8601" => validateDateTimeIso8601(prefix, js)
      case ArrayPattern(internalType) => validateArray(prefix + s" of type '[$internalType]':", internalType, js, defaultNamespace)
      case ObjectPattern(internalType) => validateObject(prefix + s" of type 'map[$internalType]':", internalType, js, defaultNamespace)
      case _ => js.validNec
    }
  }

  def validateString(prefix: String, js: JsValue): ValidatedNec[String, JsString] = {
    js match {
      case v: JsArray => {
        v.value.size match {
          case 1 => validateString(prefix, v.value.head)
          case _ => s"$prefix must be a string and not an array".invalidNec
        }
      }
      case v: JsBoolean => JsString(v.value.toString).validNec
      case JsNull => s"$prefix must be a string and not null".invalidNec
      case v: JsNumber => JsString(v.value.toString).validNec
      case _: JsObject => s"$prefix must be a string and not an object".invalidNec
      case v: JsString => v.validNec
    }
  }

  def validateArray(prefix: String, internalType: String, js: JsValue, defaultNamespace: Option[String]): ValidatedNec[String, JsArray] = {
    js match {
      case v: JsArray => {
        v.value.zipWithIndex.map { case (el, index) =>
          validate(internalType, el, defaultNamespace = defaultNamespace, prefix = Some(prefix + s" element in position[$index]"))
        }.toList.traverse(identity).map(JsArray(_))
      }
      case JsNull => s"$prefix must be an array and not null".invalidNec
      case v => validate(internalType, v, defaultNamespace = defaultNamespace, prefix =  Some(prefix + s" element in position[0]")).map(t => JsArray(Seq(t)))
    }
  }

  def validateObject(prefix: String, internalType: String, js: JsValue, defaultNamespace: Option[String]): ValidatedNec[String, JsObject] = {
    js match {
      case v: JsArray => {
        v.value.size match {
          case 1 => validateObject(prefix, internalType, v.value.head, defaultNamespace)
          case _ => s"$prefix must be an object and not an array".invalidNec
        }
      }
      case _: JsBoolean => s"$prefix must be an object and not a boolean".invalidNec
      case JsNull => s"$prefix must be an object and not null".invalidNec
      case _: JsNumber => s"$prefix must be an object and not a number".invalidNec
      case v: JsObject => {
        v.fields.foldLeft(v.validNec[String]) { case (agg, (name, el)) =>
          agg.andThen { agg =>
            validate(internalType, el, defaultNamespace = defaultNamespace, prefix = Some(prefix + s" element[$name]")).map { jsValue =>
             agg ++ Json.obj(name -> jsValue)
            }
          }
        }
      }
      case _: JsString => s"$prefix must be an object and not a string".invalidNec
    }
  }

  def validateInteger(prefix: String, js: JsValue): ValidatedNec[String, JsNumber] = {
    js match {
      case v: JsArray => {
        v.value.size match {
          case 1 => validateInteger(prefix, v.value.head)
          case _ => s"$prefix must be an integer and not an array".invalidNec
        }
      }
      case _: JsBoolean => s"$prefix must be an integer and not a boolean".invalidNec
      case JsNull => s"$prefix must be an integer and not null".invalidNec
      case v: JsNumber => v.asOpt[Int] match {
        case None => s"$prefix must be a valid integer".invalidNec
        case Some(_) => v.validNec
      }
      case _: JsObject => s"$prefix must be an integer and not an object".invalidNec
      case v: JsString => {
        Try {
          v.value.toInt
        } match {
          case Success(num) => JsNumber(num).validNec
          case Failure(_) => s"$prefix must be a valid integer".invalidNec
        }
      }
    }
  }

  def validateDouble(prefix: String, js: JsValue): ValidatedNec[String, JsNumber] = {
    js match {
      case v: JsArray => {
        v.value.size match {
          case 1 => validateDouble(prefix, v.value.head)
          case _ => s"$prefix must be a double and not an array".invalidNec
        }
      }
      case _: JsBoolean => s"$prefix must be a double and not a boolean".invalidNec
      case JsNull => s"$prefix must be a double and not null".invalidNec
      case v: JsNumber => v.asOpt[Double] match {
        case None => s"$prefix must be a valid double".invalidNec
        case Some(_) => v.validNec
      }
      case _: JsObject => s"$prefix must be a double and not an object".invalidNec
      case v: JsString => {
        Try {
          // String must be convertible to Double *and* BigDecimal, to catch "NaN".
          v.value.toDouble
          BigDecimal(v.value)
        } match {
          case Success(num) => JsNumber(num).validNec
          case Failure(_) => s"$prefix must be a valid double".invalidNec
        }
      }
    }
  }

  def validateDecimal(prefix: String, js: JsValue): ValidatedNec[String, JsNumber] = {
    js match {
      case v: JsArray => {
        v.value.size match {
          case 1 => validateDecimal(prefix, v.value.head)
          case _ => s"$prefix must be a decimal and not an array".invalidNec
        }
      }
      case _: JsBoolean => s"$prefix must be a decimal and not a boolean".invalidNec
      case JsNull => s"$prefix must be a decimal and not null".invalidNec
      case v: JsNumber => v.asOpt[BigDecimal] match {
        case None => s"$prefix must be a valid decimal".invalidNec
        case Some(_) => v.validNec
      }
      case _: JsObject => s"$prefix must be a decimal and not an object".invalidNec
      case v: JsString => {
        Try {
          BigDecimal.apply(v.value)
        } match {
          case Success(num) => JsNumber(num).validNec
          case Failure(_) => s"$prefix must be a valid decimal".invalidNec
        }
      }
    }
  }

  def validateLong(prefix: String, js: JsValue): ValidatedNec[String, JsNumber] = {
    js match {
      case v: JsArray => {
        v.value.size match {
          case 1 => validateLong(prefix, v.value.head)
          case _ => s"$prefix must be a long and not an array".invalidNec
        }
      }
      case _: JsBoolean => s"$prefix must be a long and not a boolean".invalidNec
      case JsNull => s"$prefix must be a long and not null".invalidNec
      case v: JsNumber => v.asOpt[Long] match {
        case None => s"$prefix must be a valid long".invalidNec
        case Some(_) => v.validNec
      }
      case _: JsObject => s"$prefix must be a long and not an object".invalidNec
      case v: JsString => {
        Try {
          v.value.toLong
        } match {
          case Success(num) => JsNumber(num).validNec
          case Failure(_) => s"$prefix must be a valid long".invalidNec
        }
      }
    }
  }

  def validateUuid(prefix: String, js: JsValue): ValidatedNec[String, JsString] = {
    js match {
      case v: JsArray => {
        v.value.size match {
          case 1 => validateUuid(prefix, v.value.head)
          case _ => s"$prefix must be a UUID and not an array".invalidNec
        }
      }
      case _: JsBoolean => s"$prefix must be a UUID and not a boolean".invalidNec
      case JsNull => s"$prefix must be a UUID and not null".invalidNec
      case _: JsNumber => s"$prefix must be a UUID and not a number".invalidNec
      case _: JsObject => s"$prefix must be a UUID and not an object".invalidNec
      case v: JsString => {
        Try {
          java.util.UUID.fromString(v.value)
        } match {
          case Success(uuid) => JsString(uuid.toString).validNec
          case Failure(_) => s"$prefix must be a valid UUID".invalidNec
        }
      }
    }
  }

  def validateDateIso8601(prefix: String, js: JsValue): ValidatedNec[String, JsString] = {
    js match {
      case v: JsArray => {
        v.value.size match {
          case 1 => validateDateIso8601(prefix, v.value.head)
          case _ => s"$prefix must be a valid ISO 8601 date and not an array".invalidNec
        }
      }
      case _: JsBoolean => s"$prefix must be a valid ISO 8601 date and not a boolean".invalidNec
      case JsNull => s"$prefix must be a valid ISO 8601 date and not null".invalidNec
      case _: JsNumber => s"$prefix must be a valid ISO 8601 date and not a number".invalidNec
      case _: JsObject => s"$prefix must be a valid ISO 8601 date and not an object".invalidNec
      case v: JsString => {
        Try {
          ISODateTimeFormat.yearMonthDay.parseLocalDate(v.value)
        } match {
          case Success(_) => JsString(v.value.toString).validNec
          case Failure(_) => s"$prefix must be a valid ISO 8601 date. Example: '2017-07-24'".invalidNec
        }
      }
    }
  }

  def validateDateTimeIso8601(prefix: String, js: JsValue): ValidatedNec[String, JsString] = {
    js match {
      case v: JsArray => {
        v.value.size match {
          case 1 => validateDateTimeIso8601(prefix, v.value.head)
          case _ => s"$prefix must be a valid ISO 8601 datetime and not an array".invalidNec
        }
      }
      case _: JsBoolean => s"$prefix must be a valid ISO 8601 datetime and not a boolean".invalidNec
      case JsNull => s"$prefix must be a valid ISO 8601 datetime and not null".invalidNec
      case _: JsNumber => s"$prefix must be a valid ISO 8601 datetime and not a number".invalidNec
      case _: JsObject => s"$prefix must be a valid ISO 8601 datetime and not an object".invalidNec
      case v: JsString => {
        Try {
          ISODateTimeFormat.dateTimeParser.parseDateTime(v.value)
        } match {
          case Success(_) => JsString(v.value.toString).validNec
          case Failure(_) => s"$prefix must be a valid ISO 8601 datetime. Example: '2017-07-24T09:41:08+02:00'".invalidNec
        }
      }
    }
  }


  def validateBoolean(prefix: String, js: JsValue): ValidatedNec[String, JsBoolean] = {
    js match {
      case v: JsArray => {
        v.value.size match {
          case 1 => validateBoolean(prefix, v.value.head)
          case _ => s"$prefix must be a boolean and not an array".invalidNec
        }
      }
      case v: JsBoolean => v.validNec
      case JsNull => s"$prefix must be a boolean and not null".invalidNec
      case v: JsNumber => {
        if (Booleans.TrueValues.contains(v.value.toString)) {
          JsBoolean(true).validNec
        } else {
          if (Booleans.FalseValues.contains(v.value.toString)) {
            JsBoolean(false).validNec
          } else {
            s"$prefix must be a boolean and not a number".invalidNec
          }
        }
      }
      case _: JsObject => s"$prefix must be a boolean and not an object".invalidNec
      case v: JsString => {
        if (Booleans.TrueValues.contains(v.value.toLowerCase)) {
          JsBoolean(true).validNec
        } else {
          if (Booleans.FalseValues.contains(v.value.toLowerCase)) {
            JsBoolean(false).validNec
          } else {
            s"$prefix must be a valid boolean".invalidNec
          }
        }
      }
    }
  }

}
