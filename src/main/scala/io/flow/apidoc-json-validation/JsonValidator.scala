package io.flow.lib.apidoc.json.validation

import com.bryzek.apidoc.spec.v0.models.{Enum, Model, Service, Union}
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

case class JsonValidator(val service: Service) {

  /**
    * Validates the incoming JsValue against the apidoc schema,
    * returning either human friendly validation errors or a new
    * JsValue with any conversions applied (e.g. strings to booleans,
    * numbers to string, etc. as dictated by the schema).
    */
  def validate(
    typeName: String,
    js: JsValue,
    prefix: Option[String] = None
  ): Either[Seq[String], JsValue] = {
    service.enums.find(_.name == typeName) match {
      case Some(e) => {
        validateEnum(e, js, prefix)
      }

      case None => {
        service.models.find(_.name == typeName) match {
          case Some(m) => {
            toObject(js) match {
              case Left(errors) => Left(errors)
              case Right(obj) => validateModel(m, obj, prefix)
            }
          }

          case None => {
            service.unions.find(_.name == typeName) match {
              case Some(u) => {
                toObject(js) match {
                  case Left(errors) => Left(errors)
                  case Right(obj) => validateUnion(u, obj, prefix)
                }
              }
            
              case None => {
                // may be a primitive type like 'string'
                validateType(
                  prefix.getOrElse("Type '$typeName'"),
                  typeName,
                  js
                )
              }
            }
          }
        }
      }
    }
  }


  private[this] def toObject(js: JsValue): Either[Seq[String], JsObject] = {
    js match {
      case obj: JsObject => Right(obj)
      case _ => Left(Seq(s"Expected a json object and not a '${js.getClass.getName}'"))
    }
  }

  private[this] def validateEnum(
    enum: Enum,
    js: JsValue,
    prefix: Option[String]
  ): Either[Seq[String], JsValue] = {
    validateString(prefix.getOrElse(s"Enum '${enum.name}'"), js) match {
      case Left(errors) => Left(errors)
      case Right(value) => {
        Right(value)
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
      case one :: Nil => Seq(s"Missing required field for type '${model.name}': '$one'")
      case multiple => Seq(s"Missing required fields for type '${model.name}': " + multiple.mkString("'", "', '", "'"))
    }

    val invalidTypes = js.fields.flatMap { case (name, value) =>
      model.fields.find(_.name == name) match {
        case None => {
          Nil
        }

        case Some(f) => {
          validateType(
            prefix.getOrElse(s"Type '${model.name}' field '${f.name}'"),
            f.`type`,
            value
          ) match {
            case Left(errors) => {
              errors
            }

            case Right(value) => {
              updated = updated ++ Json.obj(name -> value)
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
        // Skip validation; not a use case at flow as there is no
        // discriminator specified
        Right(js)
      }

      case Some(discriminator) => {
        (js \ discriminator).asOpt[String] match {
          case None => Left(Seq("Union type '${union.name}' requires a field named '${discriminator}'"))
          case Some(value) => validate(value, js, prefix)
        }
      }
    }
  }

  private[this] val ArrayPattern = """^\[(.+)\]$""".r
  private[this] val ObjectPattern = """^map\[(.+)\]$""".r

  /**
    * Validates the JS Value based on the expected apidoc type.
    */
  private[this] def validateType(prefix: String, typ: String, js: JsValue): Either[Seq[String], JsValue] = {
    typ match {
      case "string" => validateString(prefix, js)
      case "integer" => validateInteger(prefix, js)
      case "long" => validateLong(prefix, js)
      case "boolean" => validateBoolean(prefix, js)
      case ArrayPattern(internalType) => validateArray(prefix + s" of type '[$internalType]':", internalType, js)
      case ObjectPattern(internalType) => validateObject(prefix + s" of type 'map[$internalType]':", internalType, js)
      case other => Right(js)
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
      case JsNull => Left(Seq(s"$prefix must be a string"))
      case v: JsNumber => Right(JsString(v.value.toString))
      case v: JsObject => Left(Seq(s"$prefix must be a string and not an object"))
      case v: JsString => Right(v)
    }
  }

  def validateArray(prefix: String, internalType: String, js: JsValue): Either[Seq[String], JsArray] = {
    js match {
      case v: JsArray => {
        val eithers = v.value.zipWithIndex.map { case (el, index) =>
          validate(internalType, el, Some(prefix + s" element in position[$index]"))
        }
        eithers.forall(_.isRight) match {
          case true => Right(JsArray(eithers.map(_.right.get)))
          case false => Left(eithers.filter(_.isLeft).flatMap(_.left.get))
        }
      }
      case v: JsBoolean => Left(Seq(s"$prefix must be an array and not a boolean"))
      case JsNull => Left(Seq(s"$prefix must be an array"))
      case v: JsNumber => Left(Seq(s"$prefix must be an array and not a number"))
      case v: JsObject => Left(Seq(s"$prefix must be an array and not an object"))
      case v: JsString => Left(Seq(s"$prefix must be an array and not a string"))
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
      case v: JsBoolean => Left(Seq(s"$prefix must be an object and not a boolean"))
      case JsNull => Left(Seq(s"$prefix must be an object"))
      case v: JsNumber => Left(Seq(s"$prefix must be an object and not a number"))
      case v: JsObject => {
        val eithers: Seq[Either[Seq[String], JsObject]] = v.fields.map { case (name, el) =>
          validate(internalType, el, Some(prefix + s" element[$name]")) match {
            case Left(errors) => Left(errors)
            case Right(js) => Right(Json.obj(name -> js))
          }
        }
        eithers.forall(_.isRight) match {
          case true => {
            Right(
              eithers.map(_.right.get).foldLeft(v) { case (a, b) => a ++ b }
            )
          }
          case false => Left(eithers.filter(_.isLeft).flatMap(_.left.get))
        }
      }
      case v: JsString => Left(Seq(s"$prefix must be an object and not a string"))
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
      case v: JsBoolean => Left(Seq(s"$prefix must be an integer and not a boolean"))
      case JsNull => Left(Seq(s"$prefix must be an integer"))
      case v: JsNumber => v.asOpt[Int] match {
        case None => Left(Seq(s"$prefix must be a valid integer"))
        case Some(_) => Right(v)
      }
      case v: JsObject => Left(Seq(s"$prefix must be an integer and not a object"))
      case v: JsString => {
        Try {
          v.value.toInt
        } match {
          case Success(v) => Right(JsNumber(v))
          case Failure(_) => Left(Seq(s"$prefix must be a valid integer"))
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
      case v: JsBoolean => Left(Seq(s"$prefix must be a long and not a boolean"))
      case JsNull => Left(Seq(s"$prefix must be a long"))
      case v: JsNumber => v.asOpt[Long] match {
        case None => Left(Seq(s"$prefix must be a valid long"))
        case Some(_) => Right(v)
      }
      case v: JsObject => Left(Seq(s"$prefix must be a long and not a object"))
      case v: JsString => {
        Try {
          v.value.toLong
        } match {
          case Success(v) => Right(JsNumber(v))
          case Failure(_) => Left(Seq(s"$prefix must be a valid long"))
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
      case JsNull => Left(Seq(s"$prefix must be a boolean"))
      case v: JsNumber => Left(Seq(s"$prefix must be a boolean and not a number"))
      case v: JsObject => Left(Seq(s"$prefix must be a boolean and not a object"))
      case v: JsString => {
        Booleans.TrueValues.contains(v.value.toLowerCase) match {
          case true => Right(JsBoolean(true))
          case false => {
            Booleans.FalseValues.contains(v.value.toLowerCase) match {
              case true => Right(JsBoolean(false))
              case false => Left(Seq(s"$prefix must be a valid boolean"))
            }
          }
        }
      }
    }
  }

}
