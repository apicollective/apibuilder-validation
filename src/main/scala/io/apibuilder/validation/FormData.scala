package io.apibuilder.validation

import java.net.URLDecoder

import play.api.libs.json._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

trait EncodingOptions
object EncodingOptions {

  /**
    * If specified, when we url encode form data for arrays, we will omit indexes.
    * e.g. "variant_id[0]=bar" -> "variant_id=bar"
    * This is required for the default parameter parsing in play
    */
  case object OmitArrayIndexes extends EncodingOptions
}

/**
  * Convert any url form encoded params (query or body) to a Json
  * object. Makes best guesses on types.
  */
object FormData {

  private val Encoding: String = "UTF-8"

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
  def toEncoded(
    js: JsValue,
    keys: Seq[String] = Nil,
    options: Set[EncodingOptions] = Set.empty
  ): String = {
    def urlEncode(s: String): String = java.net.URLEncoder.encode(s, Encoding)
    def encodeIt(value: String, keys: Seq[String]): String = encode(urlEncode(value), keys)

    js match {
      case o: JsObject => {
        o.value.map { case (key, value) =>
          toEncoded(value, keys ++ Seq(key), options = options)
        }.mkString("&")
      }
      case o: JsArray => {
        o.value.zipWithIndex.map { case (v, i) =>
          val finalKeys = if (options.contains(EncodingOptions.OmitArrayIndexes)) {
            keys
          } else {
            keys ++ Seq(i.toString)
          }
          toEncoded(v, keys = finalKeys, options = options)
        }.mkString("&")
      }
      case o: JsString => encodeIt(o.value, keys)
      case o: JsBoolean => encodeIt(o.value.toString, keys)
      case o: JsNumber => encodeIt(o.value.toString, keys)
      case JsNull => encodeIt("", keys)
    }
  }

  private def encode(value: String, keys: Seq[String]): String = {
    keys.toList match {
      case Nil => value
      case one :: rest => {
        s"%s=%s".format(buildKey(one, rest), value)
      }
    }
  }

  @scala.annotation.tailrec
  private def buildKey(result: String, values: Seq[String]): String = {
    values.toList match {
      case Nil => result
      case one :: rest => buildKey(s"$result[$one]", rest)
    }
  }

  /**
    * Parses a url encoded string into a Json Object
    */
  def parseEncodedToJsObject(value: String): JsObject = {
    FormData.toJson(
      FormData.parseEncoded(value)
    )
  }

  def parseEncoded(value: String): Map[String, Seq[String]] = {
    parseEncodedToSeq(value).groupBy(_._1).map { case (key, values) =>
      key -> values.map(_._2)
    }
  }

  /**
  * Parses a url encoded string into a map
  */
  def parseEncodedToSeq(value: String): Seq[(String, String)] = {
    val data = scala.collection.mutable.ListBuffer[(String, String)]()
    value.split("&").foreach { x =>
      x.split("=", 2).toList match {
        case key :: rest if key.nonEmpty => {
          val decodedValue = rest.headOption.getOrElse("") match {
            case "" => null
            case v => URLDecoder.decode(v, Encoding)
          }

          data.append((URLDecoder.decode(key, Encoding), decodedValue))
        }

        case _ => {
          // Ignore
        }
      }
    }
    data.toSeq
  }

  def toJson(data: Map[String, Seq[String]]): JsObject = {
    toJson(data.keys.toSeq.sorted, data)
  }

  def toJsonFromSimpleMap(data: Map[String, String]): JsObject = {
    toJson(
      data.map { case (k,v) => k -> Seq(v) }
    )
  }

  def normalize(data: Seq[(String, String)], options: Set[EncodingOptions] = Set.empty): Seq[(String, String)] = {
    val parsed = toJson(
      data.groupBy(_._1).map { case (key, values) =>
        key -> values.map(_._2)
      }
    )
    val enc = toEncoded(parsed, options = options)
    parseEncodedToSeq(enc)
  }

  @tailrec
  private def toJson(
    keys: Seq[String],
    data: Map[String, Seq[String]],
    finalObject: JsObject = Json.obj()
  ): JsObject = {
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
          mergeObjects(finalObject, thisObject)
        )
      }
    }
  }

  // Based on play deepMerge method, but also merges values of underlying arrays
  private def mergeObjects(existingObject: JsObject, otherObject: JsObject): JsObject = {
    val result = existingObject.value ++ otherObject.value.map {
      case (otherKey, otherValue) =>
        val maybeExistingValue = existingObject.value.get(otherKey)

        val newValue = (maybeExistingValue, otherValue) match {
          case (Some(e: JsObject), o: JsObject) => mergeObjects(e, o)
          case (Some(e: JsArray), o: JsArray) => mergeArrays(e, o)
          case _ => otherValue
        }

        otherKey -> newValue
    }
    JsObject(result)
  }

  /**
    * Merge two arrays, preserving order where possible. Use case is for
    * when we have two arrays sparsely populated, e.g:
    *   one: [1]
    *   two: [null, 2]
    * in this case we return [1, 2]
    */
  private def mergeArrays(one: JsArray, two: JsArray): JsArray = {
    val length = Seq(one.value.length, two.value.length).max
    JsArray(
      0.until(length).map { i =>
        (one.value.lift(i).filter(_ != JsNull), two.value.lift(i).filter(_ != JsNull)) match {
          case (Some(a), None) => a
          case (None, Some(b)) => b
          case (Some(a), Some(b)) => (a, b) match {
            case (o1: JsObject, o2: JsObject) => mergeObjects(o1, o2)
            case (o1: JsArray, o2: JsArray) => mergeArrays(o1, o2)
            case _ => a
          }
          case (None, None) => JsNull
        }
      }
    )
  }

  private val EndsWithIndexInBrackets = """^(.+)\[(\d+)\]$""".r
  private val EndsWithEmptyBrackets = """^(.+)\[\]$""".r
  private val EndsWithFieldNameInBrackets = """^(.+)\[(.+)\]$""".r

  // Given input of:
  //   locations[0][city] => Paris
  // Produce
  // { "locations": [
  //     { "city": "Paris" }
  //   ]
  // }
  @tailrec
  private def toJsonObject(key: String, value: JsValue): JsObject = {
    key match {
      case EndsWithIndexInBrackets(prefix, index) => {
        // Fill in JsNull up to our desired index to preserve the explicit
        // element order in the arrays
        toJsonObject(
          prefix,
          JsArray(
            0.until(index.toInt).map { _ => JsNull } ++ Seq(value)
          )
        )
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

  private def toJsPrimitive(value: String): JsValue = {
    value match {
      case null => JsNull
      case "true" => JsBoolean(true)
      case "false" => JsBoolean(false)
      case other => {
        toLong(other) match {
          case Some(v) => JsNumber(v)
          case None => JsString(other)
        }
      }
    }
  }

  private val AcceptableRegexp = """^\-?[0-9]+$""".r

  def toLong(value: String): Option[Long] = {
    value match {
      case AcceptableRegexp() => {
        Try {
          value.toLong
        } match {
          case Success(num) => {
            if (num.toString == value) {
              Some(num)
            } else {
              None
            }
          }
          case Failure(_) => None
        }
      }
      case _ => None
    }
  }

}
