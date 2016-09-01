package io.flow.lib.apidoc.json.validation

import com.bryzek.apidoc.spec.v0.models.{Model, Service, Union}
import play.api.libs.json._
import scala.util.{Failure, Success, Try}

case class JsonValidator(service: Service) {

  def validate(typeName: String, js: JsValue): Seq[String] = {
    service.models.find(_.name == typeName) match {
      case Some(m) => {
        toObject(js) match {
          case Left(errors) => errors
          case Right(obj) => validateModel(m, obj)
        }
      }

      case None => {
        service.unions.find(_.name == typeName) match {
          case Some(u) => {
            toObject(js) match {
              case Left(errors) => errors
              case Right(obj) => validateUnion(u, obj)
            }
          }
            
          case None => {
            Nil
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

  private[this] def validateModel(model: Model, js: JsObject): Seq[String] = {
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
          validateType(s"Field ${model.name}.${f.name}", f.`type`, value)
        }
      }
    }

    missingFields ++ invalidTypes
  }

  private[this] def validateUnion(union: Union, js: JsObject): Seq[String] = {
    (js \ "discriminator").asOpt[String] match {
      case None => Nil
      case Some(discriminator) => validate(discriminator, js)
    }
  }

  /**
    * Right now just handles primitive types
    */
  private[this] def validateType(prefix: String, typ: String, js: JsValue): Seq[String] = {
    typ match {
      case "string" => validateString(prefix, js)
      case "int" => validateInteger(prefix, js)
      case "long" => validateLong(prefix, js)
      case "boolean" => validateBoolean(prefix, js)
      case other => Nil
    }
  }

  def validateString(prefix: String, js: JsValue): Seq[String] = {
    js match {
      case v: JsArray => Seq(s"$prefix must be a string and not an array")
      case v: JsBoolean => Seq(s"$prefix must be a string and not a boolean")
      case JsNull => Nil
      case v: JsNumber => Seq(s"$prefix must be a string and not a number")
      case v: JsObject => Seq(s"$prefix must be a string and not an object")
      case v: JsString => Nil
    }
  }

  def validateInteger(prefix: String, js: JsValue): Seq[String] = {
    js match {
      case v: JsArray => Seq(s"$prefix must be a integer and not a array")
      case v: JsBoolean => Seq(s"$prefix must be a integer and not a boolean")
      case JsNull => Nil
      case v: JsNumber => v.asOpt[Int] match {
        case None => Seq(s"$prefix must be a valid integer")
        case Some(_) => Nil
      }
      case v: JsObject => Seq(s"$prefix must be a integer and not a object")
      case v: JsString => {
        Try {
          v.value.toInt
        } match {
          case Success(_) => Nil
          case Failure(_) => Seq(s"$prefix must be a valid integer")
        }
      }
    }
  }

  def validateLong(prefix: String, js: JsValue): Seq[String] = {
    js match {
      case v: JsArray => Seq(s"$prefix must be a long and not a array")
      case v: JsBoolean => Seq(s"$prefix must be a long and not a boolean")
      case JsNull => Nil
      case v: JsNumber => v.asOpt[Int] match {
        case None => Seq(s"$prefix must be a valid long")
        case Some(_) => Nil
      }
      case v: JsObject => Seq(s"$prefix must be a long and not a object")
      case v: JsString => {
        Try {
          v.value.toInt
        } match {
          case Success(_) => Nil
          case Failure(_) => Seq(s"$prefix must be a valid long")
        }
      }
    }
  }

  def validateBoolean(prefix: String, js: JsValue): Seq[String] = {
    js match {
      case v: JsArray => Seq(s"$prefix must be a boolean and not a array")
      case v: JsBoolean => Nil
      case JsNull => Nil
      case v: JsNumber => Seq(s"$prefix must be a boolean and not a number")
      case v: JsObject => Seq(s"$prefix must be a boolean and not a object")
      case v: JsString => {
        v.value.toLowerCase match {
          case "t" | "true" => Nil
          case "f" | "false" => Nil
          case _ => Seq(s"$prefix must be a valid boolean")
        }
      }
    }
  }


}
