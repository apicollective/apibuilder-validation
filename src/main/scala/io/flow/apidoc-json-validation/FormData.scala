package io.flow.lib.apidoc.json.validation

import java.net.URLDecoder

import play.api.libs.json._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

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
    * number[]=100379876543&number[]=WT65xSPLX-SPT-5
    *
    * into
    *
    * number=100379876543&number=WT65xSPLX-SPT-5
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
    *             build up nested keys (e.g. user[first] for maps)
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
        case key :: rest if key.nonEmpty => {
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
    toJson(data.keys.toSeq, data)
  }

  @tailrec
  private[this] def toJson(
    keys: Seq[String],
    data: Map[String, Seq[String]],
    finalObject: JsObject = Json.obj()
  ): JsObject = {
    println(s"toJson: keys[${keys.mkString(", ")}] finalObject: $finalObject")

    keys.headOption match {
      case None => {
        finalObject
      }

      case Some(key) => {
        val jsValue = data(key) match {
          case Nil => JsNull
          case one :: Nil => toJsPrimitive(one)
          case multiple => JsArray(multiple.map(toJsPrimitive))
        }

        val i = key.lastIndexOf('[')
        val thisObject = if (i < 0) {
          Json.obj(key -> jsValue)
        } else {
          toJsonObject(key, jsValue)
        }

        toJson(
          keys.tail,
          data,
          finalObject.deepMerge(thisObject)
        )
      }
    }
  }

  private[this] val EndsWithIndexInBrackets = """^(.+)\[(\d+)\]$""".r
  private[this] val EndsWithEmptyBrackets = """^(.+)\[\]$""".r
  private[this] val EndsWithFieldNameInBrackets = """^(.+)\[(.+)\]$""".r

  // Given input of:
  //   locations[0][city] => Paris
  // Produce
  // { "locations": [
  //     { "city": "Paris" }
  //   ]
  // }
  @tailrec
  private[this] def toJsonObject(key: String, value: JsValue): JsObject = {
    println(s"toJsonObject key[$key] value: $value")

    key match {
      case EndsWithIndexInBrackets(prefix, index) => {
        println(s"TODO: prefix[$prefix] index[$index]")
        toJsonObject(prefix, JsArray(Seq(value)))
      }

      case EndsWithEmptyBrackets(prefix) => {
        value match {
          case _: JsArray => toJsonObject(prefix, value)
          case _ => toJsonObject(prefix, JsArray(Seq(value)))
        }
      }

      case EndsWithFieldNameInBrackets(prefix, name) => {
        toJsonObject(prefix, Json.obj(name -> value))
      }

      case _ => {
        Json.obj(key -> value)
      }
    }
  }

  private[this] def toJsPrimitive(value: String): JsValue = {
    value match {
      case "true" => JsBoolean(true)
      case "false" => JsBoolean(false)
      case other => {
        toNumber(other) match {
          case Some(v) => JsNumber(v)
          case None => JsString(other)
        }
      }
    }
  }

  private[this] val AcceptableRegexp = """^\-?[0-9]+$""".r

  def toNumber(value: String): Option[BigDecimal] = {
    value match {
      case AcceptableRegexp() => {
        Try {
          BigDecimal(value)
        } match {
          case Success(num) => Some(num)
          case Failure(_) => None
        }
      }
      case _ => None
    }
  }

}
