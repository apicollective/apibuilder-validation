package io.flow.lib.apidoc.json.validation

import com.bryzek.apidoc.spec.v0.models.{Model, Service, Union}
import play.api.libs.json._
import scala.util.{Failure, Success, Try}

case class JsonValidator(service: Service) {

  /**
    * Validates the incoming JsValue against the apidoc schema,
    * returning either human friendly validation errors or a new
    * JsValue with any conversions applied (e.g. strings to booleans,
    * numbers to string, etc. as dictated by the schema).
    */
  def validate(typeName: String, js: JsValue): Either[Seq[String], JsValue] = {
    service.models.find(_.name == typeName) match {
      case Some(m) => {
        toObject(js) match {
          case Left(errors) => Left(errors)
          case Right(obj) => validateModel(m, obj)
        }
      }

      case None => {
        service.unions.find(_.name == typeName) match {
          case Some(u) => {
            toObject(js) match {
              case Left(errors) => Left(errors)
              case Right(obj) => validateUnion(u, obj)
            }
          }
            
          case None => {
            // Could not find model; just return original js value
            Right(js)
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

  private[this] def validateModel(model: Model, js: JsObject): Either[Seq[String], JsValue] = {
    var updated = Json.obj()

    val missingFields = model.fields.filter(_.required).filter { f =>
      (js \ f.name).toOption.isEmpty
    }.map(_.name).toList match {
      case Nil => Nil
      case one :: Nil => Seq(s"Missing required field for ${model.name}: '$one'")
      case multiple => Seq(s"Missing required fields for ${model.name}: " + multiple.mkString("'", "', '", "'"))
    }

    val invalidTypes = js.fields.flatMap { case (name, value) =>
      model.fields.find(_.name == name) match {
        case None => {
          Nil
        }

        case Some(f) => {
          validateType(s"Field ${model.name}.${f.name}", f.`type`, value) match {
            case Left(errors) => errors
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

  private[this] def validateUnion(union: Union, js: JsObject): Either[Seq[String], JsValue] = {
    (js \ "discriminator").asOpt[String] match {
      case None => Right(js)
      case Some(discriminator) => validate(discriminator, js)
    }
  }

  /**
    * Right now just handles primitive types
    */
  private[this] def validateType(prefix: String, typ: String, js: JsValue): Either[Seq[String], JsValue] = {
    typ match {
      case "string" => validateString(prefix, js)
      case "int" => validateInteger(prefix, js)
      case "long" => validateLong(prefix, js)
      case "boolean" => validateBoolean(prefix, js)
      case other => Right(js)
    }
  }

  def validateString(prefix: String, js: JsValue): Either[Seq[String], JsValue] = {
    js match {
      case v: JsArray => Left(Seq(s"$prefix must be a string and not an array"))
      case v: JsBoolean => Left(Seq(s"$prefix must be a string and not a boolean"))
      case JsNull => Right(js)
      case v: JsNumber => Right(JsString(v.value.toString))
      case v: JsObject => Left(Seq(s"$prefix must be a string and not an object"))
      case v: JsString => Right(js)
    }
  }

  def validateInteger(prefix: String, js: JsValue): Either[Seq[String], JsValue] = {
    js match {
      case v: JsArray => Left(Seq(s"$prefix must be a integer and not a array"))
      case v: JsBoolean => Left(Seq(s"$prefix must be a integer and not a boolean"))
      case JsNull => Right(js)
      case v: JsNumber => v.asOpt[Int] match {
        case None => Left(Seq(s"$prefix must be a valid integer"))
        case Some(_) => Right(js)
      }
      case v: JsObject => Left(Seq(s"$prefix must be a integer and not a object"))
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

  def validateLong(prefix: String, js: JsValue): Either[Seq[String], JsValue] = {
    js match {
      case v: JsArray => Left(Seq(s"$prefix must be a long and not a array"))
      case v: JsBoolean => Left(Seq(s"$prefix must be a long and not a boolean"))
      case JsNull => Right(js)
      case v: JsNumber => v.asOpt[Long] match {
        case None => Left(Seq(s"$prefix must be a valid long"))
        case Some(_) => Right(js)
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

  def validateBoolean(prefix: String, js: JsValue): Either[Seq[String], JsValue] = {
    js match {
      case v: JsArray => Left(Seq(s"$prefix must be a boolean and not a array"))
      case v: JsBoolean => Right(js)
      case JsNull => Right(js)
      case v: JsNumber => Left(Seq(s"$prefix must be a boolean and not a number"))
      case v: JsObject => Left(Seq(s"$prefix must be a boolean and not a object"))
      case v: JsString => {
        v.value.toLowerCase match {
          case "t" | "true" => Right(JsBoolean(true))
          case "f" | "false" => Right(JsBoolean(false))
          case _ => Left(Seq(s"$prefix must be a valid boolean"))
        }
      }
    }
  }


}
