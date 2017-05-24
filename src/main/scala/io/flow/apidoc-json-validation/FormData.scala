package io.flow.lib.apidoc.json.validation

import java.net.URLDecoder

import play.api.libs.json._

/**
  * Convert any url form encoded params (query or body) to a Json
  * object. Makes best guesses on types.
  */
object FormData {

  private[this] val Encoding: String = "UTF-8"

  /**
    * Given a url encoded string, parses it and then reformats as url
    * encoded. Main use case is to turn things like:
    * 
    *     number[]=100379876543&number[]=WT65xSPLX-SPT-5
    * 
    * into
    * 
    *     number=100379876543&number=WT65xSPLX-SPT-5
    */
  def rewriteEncoded(value: String): String = {
    toEncoded(
      toJson(
        parseEncoded(value)
      )
    )
  }

  /**
    * Converts the specified js value into a url form encoded string,
    * recursively through all types.
    * 
    * @param keys Keeps track of the top level keys we are parsing to
    *        build up nested keys (e.g. user[first] for maps)
    */
  def toEncoded(js: JsValue, keys: Seq[String] = Nil): String = {
    js match {
      case o: JsObject => {
        o.value.map { case (key, value) =>
          toEncoded(value, keys ++ Seq(key))
        }.mkString("&")
      }
      case o: JsArray => {
        o.value.map { v =>
          toEncoded(v, keys)
        }.mkString("&")
      }
      case o: JsString => encode(o.value, keys)
      case o: JsBoolean => encode(o.value.toString, keys)
      case o: JsNumber => encode(o.value.toString, keys)
      case JsNull => encode("", keys)
      case other => encode(other.toString, keys)
    }
  }

  private[this] def encode(value: String, keys: Seq[String] = Nil): String = {
    keys.toList match {
      case Nil => value
      case one :: rest => {
        s"%s=%s".format(buildKey(one, rest), value)
      }
    }
  }

  @scala.annotation.tailrec
  private[this] def buildKey(result: String, values: Seq[String]): String = {
    values.toList match {
      case Nil => result
      case one :: rest => buildKey(s"$result[$one]", rest)
    }
  }

  def parseEncoded(value: String): Map[String, Seq[String]] = {
    val data = scala.collection.mutable.Map[String, Seq[String]]()
    value.split("&").foreach { x =>
      x.split("=", 2).toList match {
        case key :: rest => {
          val decodedValue = URLDecoder.decode(rest.headOption.getOrElse(""), Encoding)
          val values = data.get(key) match {
            case None => Seq(decodedValue)
            case Some(existing) => existing ++ Seq(decodedValue)
          }
          data += URLDecoder.decode(key, Encoding) -> values
        }

        case _ => {
          // Ignore
        }
      }
    }
    data.toMap
  }

  def toJson(data: Map[String, Seq[String]]): JsObject = {
    val nested = data.map{ case (key, values) =>
      key.split("\\[").foldRight(
        if(key.contains("[]")) {
          Json.toJson(values)  //take seq for arrays
        } else {
          values match {
            case Nil => Json.toJson("")
            case one :: Nil => Json.toJson(one)
            case multiple => Json.toJson(values)
          }
        }
      ){ case (newKey, v) =>
        val newVal = {
          val js = (v \ "").getOrElse(v)

          //convert '{key: val}' to '[{key: val}]' if previous key specifies array type, otherwise nothing
          if (newKey == "]") {
            if (!js.toString.startsWith("[")) {
              val s = (v \ "").getOrElse(v).toString.
                replaceFirst("\\{", "[{").
                reverse.
                replaceFirst("\\}", "]}").
                reverse

              Json.toJson(Json.parse(s))
            } else {
              js
            }

          } else {
            js
          }
        }

        Json.obj(newKey.replace("]", "") -> newVal)
      }
    }

    nested.foldLeft(Json.obj()){ case (a, b) => a.deepMerge(b.as[JsObject]) }
  }

}
