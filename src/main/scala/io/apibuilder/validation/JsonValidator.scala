package io.apibuilder.validation

import io.apibuilder.spec.v0.models.{Enum, Model, Service, Union}
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

  val TrueValues = Seq("t", "true", "y", "yes", "on", "1", "trueclass")
  val FalseValues = Seq("f", "false", "n", "no", "off", "0", "falseclass")

}

object JsonValidator {
  def apply(service: Service): JsonValidator = JsonValidator(Seq(service))
}

case class JsonValidator(services: Seq[Service]) {
  assert(services.nonEmpty, s"Must have at least one service")

  def findType(name: String): Option[ApibuilderType] = {
    val typeName = TypeName(name)
    typeName.namespace match {
      case None => {
        // find first service with this type defined
        services.flatMap { s =>
          findType(s, typeName.name)
        }.headOption
      }

      case Some(ns) => {
        services.find(_.namespace == ns).flatMap { s =>
          findType(s, typeName.name)
        }
      }
    }
  }

  private[this] def findType(service: Service, typeName: String): Option[ApibuilderType] = {
    service.enums.find(_.name == typeName) match {
      case Some(e) => Some(ApibuilderType.Enum(service.namespace, e))
      case None => {
        service.models.find(_.name == typeName) match {
          case Some(m) => Some(ApibuilderType.Model(service.namespace, m))
          case None => {
            service.unions.find(_.name == typeName) match {
              case Some(u) => Some(ApibuilderType.Union(service.namespace, u))
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
    prefix: Option[String] = None
  ): Either[Seq[String], JsValue] = {
    findType(typeName) match {
      case None => {
        // may be a primitive type like 'string'
        validateApiBuilderType(
          prefix.getOrElse(typeName),
          typeName,
          js
        )
      }

      case Some(t) => {
        t match {
          case ApibuilderType.Enum(_, e) => {
            validateEnum(prefix.getOrElse("Body"), e, js)
          }

          case ApibuilderType.Model(_, m) => {
            toObject(prefix.getOrElse("Body"), js) match {
              case Left(errors) => Left(errors)
              case Right(obj) => validateModel(m, obj, prefix)
            }
          }

          case ApibuilderType.Union(_, u) => {
            toObject(prefix.getOrElse("Body"), js) match {
              case Left(errors) => Left(errors)
              case Right(obj) => validateUnion(u, obj, prefix)
            }
          }
        }
      }
    }
  }

  private[this] def toObject(prefix: String, js: JsValue): Either[Seq[String], JsObject] = {
    js match {
      case _: JsArray => {
        Left(Seq(s"$prefix must be an object and not an array"))
      }
      case _: JsBoolean => Left(Seq(s"$prefix must be an object and not a boolean"))
      case JsNull => Left(Seq(s"$prefix must be an object and not null"))
      case _: JsNumber => Left(Seq(s"$prefix must be an object and not a number"))
      case v: JsObject => Right(
        // Remove null fields as there is nothing to validate there
        JsObject(
          v.value.filter { case (_, value) =>
              value match {
                case JsNull => false
                case _ => true
              }
          }
        )
      )
      case _: JsString => Left(Seq(s"$prefix must be an object and not a string"))
    }
  }

  private[this] def validateEnum(
    prefix: String,
    enum: Enum,
    js: JsValue
  ): Either[Seq[String], JsValue] = {
    validateString(prefix, js) match {
      case Left(errors) => Left(errors)
      case Right(jsString) => {
        val incomingValue = jsString.value.trim
        val valid = enum.values.map(_.name)
        valid.find { _.toLowerCase.trim == incomingValue.toLowerCase } match {
          case None => {
            Left(
              Seq(
                s"$prefix invalid value '${incomingValue}'. Valid values for the enum '${enum.name}' are: " +
                  valid.mkString("'", "', '", "'")
              )
            )
          }
          case Some(validValue) => Right(JsString(validValue))
        }
      }
    }
  }

  private[this] def validateModel(
    model: Model,
    js: JsObject,
    prefix: Option[String]
  ): Either[Seq[String], JsValue] = {
    var updated = Json.obj()

    val missingFields = model.fields.filter(_.required).filter { f =>
      (js \ f.name).toOption.isEmpty
    }.map(_.name).toList match {
      case Nil => Nil
      case one :: Nil => Seq(s"Missing required field for ${model.name}: $one")
      case multiple => Seq(s"Missing required fields for ${model.name}: " + multiple.mkString(", "))
    }

    val invalidTypes = js.fields.flatMap { case (name, value) =>
      model.fields.find(_.name == name) match {
        case None => {
          Nil
        }

        case Some(f) => {
          validate(
            typeName = f.`type`,
            js = value,
            prefix = Some(
              prefix.getOrElse(model.name) + s".${f.name}"
            )
          ) match {
            case Left(errors) => {
              errors
            }

            case Right(v) => {
              updated = updated ++ Json.obj(name -> v)
              Nil
            }
          }
        }
      }
    }

    (missingFields ++ invalidTypes).toList match {
      case Nil => Right(js ++ updated)
      case errors => Left(errors)
    }
  }

  private[this] def validateUnion(
    union: Union,
    js: JsObject,
    prefix: Option[String]
  ): Either[Seq[String], JsValue] = {
    union.discriminator match {
      case None => {
        Right(js)
      }

      case Some(discriminator) => {
        val disc = (js \ discriminator).asOpt[String]

        val unionType = disc match {
          case None => union.types.find(_.default.getOrElse(false))
          case Some(t) => union.types.find(_.`type` == t)
        }

        unionType match {
          case None => {
            disc match {
              case None => {
                Left(Seq(s"Union type '${union.name}' requires a field named '$discriminator'"))
              }
              case Some(value) => {
                Left(Seq(s"Invalid discriminator '$value' for union type '${union.name}': must be one of " + union.types.map(_.`type`).mkString("'", "', '", "'")))
              }
            }

          }

          case Some(t) => {
            assert(t.`type` != union.name, s"Specific union type name cannot match name of the union itself - else we'd recurse infinitely")
            validate(t.`type`, js, prefix)
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
  private[this] def validateApiBuilderType(prefix: String, typ: String, js: JsValue): Either[Seq[String], JsValue] = {
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
      case ArrayPattern(internalType) => validateArray(prefix + s" of type '[$internalType]':", internalType, js)
      case ObjectPattern(internalType) => validateObject(prefix + s" of type 'map[$internalType]':", internalType, js)
      case _ => Right(js)
    }
  }

  def validateString(prefix: String, js: JsValue): Either[Seq[String], JsString] = {
    js match {
      case v: JsArray => {
        v.value.size match {
          case 1 => validateString(prefix, v.value.head)
          case _ => Left(Seq(s"$prefix must be a string and not an array"))
        }
      }
      case v: JsBoolean => Right(JsString(v.value.toString))
      case JsNull => Left(Seq(s"$prefix must be a string and not null"))
      case v: JsNumber => Right(JsString(v.value.toString))
      case _: JsObject => Left(Seq(s"$prefix must be a string and not an object"))
      case v: JsString => Right(v)
    }
  }

  def validateArray(prefix: String, internalType: String, js: JsValue): Either[Seq[String], JsArray] = {
    js match {
      case v: JsArray => {
        val eithers = v.value.zipWithIndex.map { case (el, index) =>
          validate(internalType, el, Some(prefix + s" element in position[$index]"))
        }
        if (eithers.forall(_.isRight)) {
          Right(JsArray(eithers.map(_.right.get)))
        } else {
          Left(eithers.filter(_.isLeft).flatMap(_.left.get))
        }
      }
      case JsNull => Left(Seq(s"$prefix must be an array and not null"))
      case v => validate(internalType, v, Some(prefix + s" element in position[0]")) match {
        case Left(errors) => Left(errors)
        case Right(t) => Right(JsArray(Seq(t)))
      }
    }
  }

  def validateObject(prefix: String, internalType: String, js: JsValue): Either[Seq[String], JsObject] = {
    js match {
      case v: JsArray => {
        v.value.size match {
          case 1 => validateObject(prefix, internalType, v.value.head)
          case _ => Left(Seq(s"$prefix must be an object and not an array"))
        }
      }
      case _: JsBoolean => Left(Seq(s"$prefix must be an object and not a boolean"))
      case JsNull => Left(Seq(s"$prefix must be an object and not null"))
      case _: JsNumber => Left(Seq(s"$prefix must be an object and not a number"))
      case v: JsObject => {
        val eithers: Seq[Either[Seq[String], JsObject]] = v.fields.map { case (name, el) =>
          validate(internalType, el, Some(prefix + s" element[$name]")) match {
            case Left(errors) => Left(errors)
            case Right(json) => Right(Json.obj(name -> json))
          }
        }
        if (eithers.forall(_.isRight)) {
          Right(
            eithers.map(_.right.get).foldLeft(v) { case (a, b) => a ++ b }
          )
        } else {
          Left(eithers.filter(_.isLeft).flatMap(_.left.get))
        }
      }
      case _: JsString => Left(Seq(s"$prefix must be an object and not a string"))
    }
  }

  def validateInteger(prefix: String, js: JsValue): Either[Seq[String], JsNumber] = {
    js match {
      case v: JsArray => {
        v.value.size match {
          case 1 => validateInteger(prefix, v.value.head)
          case _ => Left(Seq(s"$prefix must be an integer and not an array"))
        }
      }
      case _: JsBoolean => Left(Seq(s"$prefix must be an integer and not a boolean"))
      case JsNull => Left(Seq(s"$prefix must be an integer and not null"))
      case v: JsNumber => v.asOpt[Int] match {
        case None => Left(Seq(s"$prefix must be a valid integer"))
        case Some(_) => Right(v)
      }
      case _: JsObject => Left(Seq(s"$prefix must be an integer and not an object"))
      case v: JsString => {
        Try {
          v.value.toInt
        } match {
          case Success(num) => Right(JsNumber(num))
          case Failure(_) => Left(Seq(s"$prefix must be a valid integer"))
        }
      }
    }
  }

  def validateDouble(prefix: String, js: JsValue): Either[Seq[String], JsNumber] = {
    js match {
      case v: JsArray => {
        v.value.size match {
          case 1 => validateDouble(prefix, v.value.head)
          case _ => Left(Seq(s"$prefix must be a double and not an array"))
        }
      }
      case _: JsBoolean => Left(Seq(s"$prefix must be a double and not a boolean"))
      case JsNull => Left(Seq(s"$prefix must be a double and not null"))
      case v: JsNumber => v.asOpt[Double] match {
        case None => Left(Seq(s"$prefix must be a valid double"))
        case Some(_) => Right(v)
      }
      case _: JsObject => Left(Seq(s"$prefix must be a double and not an object"))
      case v: JsString => {
        Try {
          v.value.toDouble
        } match {
          case Success(num) => Right(JsNumber(num))
          case Failure(_) => Left(Seq(s"$prefix must be a valid double"))
        }
      }
    }
  }

  def validateDecimal(prefix: String, js: JsValue): Either[Seq[String], JsNumber] = {
    js match {
      case v: JsArray => {
        v.value.size match {
          case 1 => validateDecimal(prefix, v.value.head)
          case _ => Left(Seq(s"$prefix must be a decimal and not an array"))
        }
      }
      case _: JsBoolean => Left(Seq(s"$prefix must be a decimal and not a boolean"))
      case JsNull => Left(Seq(s"$prefix must be a decimal and not null"))
      case v: JsNumber => v.asOpt[BigDecimal] match {
        case None => Left(Seq(s"$prefix must be a valid decimal"))
        case Some(_) => Right(v)
      }
      case _: JsObject => Left(Seq(s"$prefix must be a decimal and not an object"))
      case v: JsString => {
        Try {
          BigDecimal.apply(v.value)
        } match {
          case Success(num) => Right(JsNumber(num))
          case Failure(_) => Left(Seq(s"$prefix must be a valid decimal"))
        }
      }
    }
  }

  def validateLong(prefix: String, js: JsValue): Either[Seq[String], JsNumber] = {
    js match {
      case v: JsArray => {
        v.value.size match {
          case 1 => validateLong(prefix, v.value.head)
          case _ => Left(Seq(s"$prefix must be a long and not an array"))
        }
      }
      case _: JsBoolean => Left(Seq(s"$prefix must be a long and not a boolean"))
      case JsNull => Left(Seq(s"$prefix must be a long and not null"))
      case v: JsNumber => v.asOpt[Long] match {
        case None => Left(Seq(s"$prefix must be a valid long"))
        case Some(_) => Right(v)
      }
      case _: JsObject => Left(Seq(s"$prefix must be a long and not an object"))
      case v: JsString => {
        Try {
          v.value.toLong
        } match {
          case Success(num) => Right(JsNumber(num))
          case Failure(_) => Left(Seq(s"$prefix must be a valid long"))
        }
      }
    }
  }

  def validateUuid(prefix: String, js: JsValue): Either[Seq[String], JsString] = {
    js match {
      case v: JsArray => {
        v.value.size match {
          case 1 => validateUuid(prefix, v.value.head)
          case _ => Left(Seq(s"$prefix must be a UUID and not an array"))
        }
      }
      case _: JsBoolean => Left(Seq(s"$prefix must be a UUID and not a boolean"))
      case JsNull => Left(Seq(s"$prefix must be a UUID and not null"))
      case _: JsNumber => Left(Seq(s"$prefix must be a UUID and not a number"))
      case _: JsObject => Left(Seq(s"$prefix must be a UUID and not an object"))
      case v: JsString => {
        Try {
          java.util.UUID.fromString(v.value)
        } match {
          case Success(uuid) => Right(JsString(uuid.toString))
          case Failure(_) => Left(Seq(s"$prefix must be a valid UUID"))
        }
      }
    }
  }

  def validateDateIso8601(prefix: String, js: JsValue): Either[Seq[String], JsString] = {
    js match {
      case v: JsArray => {
        v.value.size match {
          case 1 => validateDateIso8601(prefix, v.value.head)
          case _ => Left(Seq(s"$prefix must be a valid ISO 8601 date and not an array"))
        }
      }
      case _: JsBoolean => Left(Seq(s"$prefix must be a valid ISO 8601 date and not a boolean"))
      case JsNull => Left(Seq(s"$prefix must be a valid ISO 8601 date and not null"))
      case _: JsNumber => Left(Seq(s"$prefix must be a valid ISO 8601 date and not a number"))
      case _: JsObject => Left(Seq(s"$prefix must be a valid ISO 8601 date and not an object"))
      case v: JsString => {
        Try {
          ISODateTimeFormat.yearMonthDay.parseLocalDate(v.value)
        } match {
          case Success(_) => Right(JsString(v.value.toString))
          case Failure(_) => Left(Seq(s"$prefix must be a valid ISO 8601 date. Example: '2017-07-24'"))
        }
      }
    }
  }

  def validateDateTimeIso8601(prefix: String, js: JsValue): Either[Seq[String], JsString] = {
    js match {
      case v: JsArray => {
        v.value.size match {
          case 1 => validateDateTimeIso8601(prefix, v.value.head)
          case _ => Left(Seq(s"$prefix must be a valid ISO 8601 datetime and not an array"))
        }
      }
      case _: JsBoolean => Left(Seq(s"$prefix must be a valid ISO 8601 datetime and not a boolean"))
      case JsNull => Left(Seq(s"$prefix must be a valid ISO 8601 datetime and not null"))
      case _: JsNumber => Left(Seq(s"$prefix must be a valid ISO 8601 datetime and not a number"))
      case _: JsObject => Left(Seq(s"$prefix must be a valid ISO 8601 datetime and not an object"))
      case v: JsString => {
        println(s"validateDateTimeIso8601 $prefix $v")
        Try {
          ISODateTimeFormat.dateTimeParser.parseDateTime(v.value)
        } match {
          case Success(_) => Right(JsString(v.value.toString))
          case Failure(_) => Left(Seq(s"$prefix must be a valid ISO 8601 datetime. Example: '2017-07-24T09:41:08+02:00'"))
        }
      }
    }
  }


  def validateBoolean(prefix: String, js: JsValue): Either[Seq[String], JsBoolean] = {
    js match {
      case v: JsArray => {
        v.value.size match {
          case 1 => validateBoolean(prefix, v.value.head)
          case _ => Left(Seq(s"$prefix must be a boolean and not an array"))
        }
      }
      case v: JsBoolean => Right(v)
      case JsNull => Left(Seq(s"$prefix must be a boolean and not null"))
      case v: JsNumber => {
        if (Booleans.TrueValues.contains(v.value.toString)) {
          Right(JsBoolean(true))
        } else {
          if (Booleans.FalseValues.contains(v.value.toString)) {
            Right(JsBoolean(false))
          } else {
            Left(Seq(s"$prefix must be a boolean and not a number"))
          }
        }
      }
      case _: JsObject => Left(Seq(s"$prefix must be a boolean and not an object"))
      case v: JsString => {
        if (Booleans.TrueValues.contains(v.value.toLowerCase)) {
          Right(JsBoolean(true))
        } else {
          if (Booleans.FalseValues.contains(v.value.toLowerCase)) {
            Right(JsBoolean(false))
          } else {
            Left(Seq(s"$prefix must be a valid boolean"))
          }
        }
      }
    }
  }

}
