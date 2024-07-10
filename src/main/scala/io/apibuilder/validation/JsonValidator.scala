package io.apibuilder.validation

import cats.implicits._
import java.util.concurrent.ConcurrentHashMap

import cats.implicits._
import cats.data.ValidatedNec
import io.apibuilder.spec.v0.models.UnionType
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
  def apply(service: ApiBuilderService): JsonValidator = new JsonValidator(ValidatedJsonValidator(service))
  def apply(services: List[ApiBuilderService]): JsonValidator = new JsonValidator(ValidatedJsonValidator(services))
}

case class JsonValidator(validator: ValidatedJsonValidator) {
  import io.apibuilder.validation.util.Implicits._

  def findType(name: String, defaultNamespace: Option[String]): Seq[AnyType] =
    validator.findType(name, defaultNamespace)

  def findType(defaultNamespace: String, name: String): Seq[AnyType] =
    validator.findType(defaultNamespace, name)

  def validate(typeName: String, js: JsValue, defaultNamespace: Option[String], prefix: Option[String] = None): ValidatedNec[String, JsValue] =
    validator.validate(typeName, js, defaultNamespace, prefix)

  def validateType(typ: AnyType, js: JsValue, prefix: Option[String] = None): ValidatedNec[String, JsValue] =
    validator.validateType(typ, js, prefix)

  def validateString(prefix: String, js: JsValue): ValidatedNec[String, JsString] =
    validator.validateString(prefix, js)

  def validateArray(prefix: String, internalType: String, js: JsValue, defaultNamespace: Option[String]): ValidatedNec[String, JsArray] =
    validator.validateArray(prefix, internalType, js, defaultNamespace)

  def validateObject(prefix: String, internalType: String, js: JsValue, defaultNamespace: Option[String]): ValidatedNec[String, JsObject] =
    validator.validateObject(prefix, internalType, js, defaultNamespace)

  def validateInteger(prefix: String, js: JsValue): ValidatedNec[String, JsNumber] =
    validator.validateInteger(prefix, js)

  def validateDouble(prefix: String, js: JsValue): ValidatedNec[String, JsNumber] =
    validator.validateDouble(prefix, js)

  def validateDecimal(prefix: String, js: JsValue): ValidatedNec[String, JsNumber] =
    validator.validateDecimal(prefix, js)

  def validateLong(prefix: String, js: JsValue): ValidatedNec[String, JsNumber] =
    validator.validateLong(prefix, js)

  def validateUuid(prefix: String, js: JsValue): ValidatedNec[String, JsString] =
    validator.validateUuid(prefix, js)

  def validateDateIso8601(prefix: String, js: JsValue): ValidatedNec[String, JsString] =
    validator.validateDateIso8601(prefix, js)

  def validateDateTimeIso8601(prefix: String, js: JsValue): ValidatedNec[String, JsString] =
    validator.validateDateTimeIso8601(prefix, js)

  def validateBoolean(prefix: String, js: JsValue): ValidatedNec[String, JsBoolean] =
    validator.validateBoolean(prefix, js)
}

object ValidatedJsonValidator {
  def apply(service: ApiBuilderService): ValidatedJsonValidator = ValidatedJsonValidator(List(service))
}

case class ValidatedJsonValidator(services: List[ApiBuilderService]) {
  assert(services.nonEmpty, s"Must have at least one service")

  def findType(name: String, defaultNamespace: Option[String]): Seq[AnyType] = {
    defaultNamespace match {
      case None => {
        ScalarType.fromName(name) match {
          case Some(st) => Seq(st)
          case None => {
            services.map { service =>
              TypeName.parse(defaultNamespace = service.namespace, name = name)
            }.distinct.flatMap { typeName =>
              findType(defaultNamespace = typeName.namespace, name = typeName.name)
            }
          }
        }
      }
      case Some(ns) => {
        findType(ns, name)
      }
    }
  }

  def findType(defaultNamespace: String, name: String): Seq[AnyType] = {
    findType(TypeName.parse(defaultNamespace = defaultNamespace, name = name))
  }

  private val cache = new ConcurrentHashMap[TypeName, Seq[AnyType]]()

  private def findType(typeName: TypeName): Seq[AnyType] = {
    cache.computeIfAbsent(
      typeName,
      _ => {
        ScalarType.fromName(typeName.name) match {
          case Some(st) => Seq(st)
          case None => {
            services.filter(_.namespace.equalsIgnoreCase(typeName.namespace)).flatMap { service =>
              findType(service, typeName.name)
            }
          }
        }
      }
    )
  }

  private def findType(service: ApiBuilderService, typeName: String): Option[AnyType] = {
    service.enums.find(_.name.equalsIgnoreCase(typeName)) orElse {
      service.models.find(_.name.equalsIgnoreCase(typeName)) orElse {
        service.unions.find(_.name.equalsIgnoreCase(typeName))
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
        // may be a collection type like '[string]'
        validateTypeFromName(
          prefix.getOrElse("value"),
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
    typ: AnyType,
    js: JsValue,
    prefix: Option[String] = None
  ): ValidatedNec[String, JsValue] = {
    typ match {
      case st: ScalarType => validateScalar(prefix.getOrElse("value"), st, js)

      case e: ApiBuilderType.Enum => {
        validateEnum(prefix.getOrElse("Body"), e, js)
      }

      case m: ApiBuilderType.Model => {
        toObject(prefix.getOrElse("Body"), js).andThen(validateModel(m, _, prefix))
      }

      case i: ApiBuilderType.Interface => {
        toObject(prefix.getOrElse("Body"), js).andThen(validateInterface(i, _, prefix))
      }

      case u: ApiBuilderType.Union => {
        toObject(prefix.getOrElse("Body"), js).andThen(validateUnion(u, _, prefix))
      }
    }
  }

  private def toObject(prefix: String, js: JsValue): ValidatedNec[String, JsObject] = {
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

  private def validateEnum(
    prefix: String,
    typ: ApiBuilderType.Enum,
    js: JsValue
  ): ValidatedNec[String, JsValue] = {
    validateString(prefix, js).andThen { jsString =>
        val incomingValue = jsString.value.trim
        val valid = typ.`enum`.values.flatMap { e =>
          Seq(e.name) ++ e.value.toSeq
        }
        valid.find { _.toLowerCase.trim == incomingValue.toLowerCase } match {
          case None =>
            (s"$prefix invalid value '${incomingValue}'. Valid values for the enum '${typ.`enum`.name}' are: " + valid.distinct.mkString("'", "', '", "'")).invalidNec
          case Some(validValue) =>
            JsString(validValue).validNec
        }
    }
  }

  private def validateModel(
    typ: ApiBuilderType.Model,
    js: JsObject,
    prefix: Option[String]
  ): ValidatedNec[String, JsValue] = {
    validateFields(typ.name, typ.fields, js, prefix)
  }

  private def validateInterface(
    typ: ApiBuilderType.Interface,
    js: JsObject,
    prefix: Option[String]
  ): ValidatedNec[String, JsValue] = {
    validateFields(typ.name, typ.fields, js, prefix)
  }

  private def validateFields(
    parentTypeName: String,
    fields: Seq[ApiBuilderField],
    js: JsObject,
    prefix: Option[String]
  ): ValidatedNec[String, JsValue] = {
    val missingFields = fields.map(_.field).filter(_.required).filter { f =>
      (js \ f.name).toOption.isEmpty
    }.map(_.name).toList

    val missingFieldsV = missingFields match {
      case Nil => ().validNec[String]
      case one :: Nil => s"Missing required field for $parentTypeName: $one".invalidNec[Unit]
      case multiple => (s"Missing required fields for $parentTypeName: " + multiple.mkString(", ")).invalidNec[Unit]
    }

    val invalidTypesV: ValidatedNec[String, JsObject] = js.fields.foldLeft(Json.obj().validNec[String]) { case (agg, (name, value)) =>
      fields.find(_.field.name == name) match {
        case None => {
          agg
        }

        case Some(f) => {
          agg.andThen { agg =>
            validate(
              typeName = f.field.`type`,
              js = value,
              prefix = Some(
                prefix.getOrElse(parentTypeName) + s".${f.field.name}"
              ),
              defaultNamespace = Some(f.service.namespace)
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

  private def validateUnion(
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

  private val ArrayPattern = """^\[(.+)\]$""".r
  private val ObjectPattern = """^map\[(.+)\]$""".r

  /**
    * Validates the JS Value based on the expected API Builder type.
    */
  private def validateTypeFromName(
    prefix: String,
    typ: String,
    js: JsValue,
    defaultNamespace: Option[String]
  ): ValidatedNec[String, JsValue] = {
    typ match {
      case ArrayPattern(internalType) => validateArray(prefix + s" of type '[$internalType]':", internalType, js, defaultNamespace)
      case ObjectPattern(internalType) => validateObject(prefix + s" of type 'map[$internalType]':", internalType, js, defaultNamespace)
      case _ => {
        ScalarType.fromName(typ) match {
          case None => js.validNec
          case Some(st) => validateScalar(prefix, st, js)
        }
      }
    }
  }

  private def validateScalar(prefix: String, scalarType: ScalarType, js: JsValue): ValidatedNec[String, JsValue] = {
    import ScalarType._
    scalarType match {
      case FloatType | JsonType | ObjectType | UnitType => {
        // TODO: Add validation for these types
        js.validNec
      }
      case StringType => validateString(prefix, js)
      case IntegerType => validateInteger(prefix, js)
      case LongType => validateLong(prefix, js)
      case BooleanType => validateBoolean(prefix, js)
      case DoubleType => validateDouble(prefix, js)
      case DecimalType => validateDecimal(prefix, js)
      case UuidType => validateUuid(prefix, js)
      case DateIso8601Type => validateDateIso8601(prefix, js)
      case DateTimeIso8601Type => validateDateTimeIso8601(prefix, js)
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
          case Success(_) => JsString(v.value).validNec
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
          case Success(_) => JsString(v.value).validNec
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
