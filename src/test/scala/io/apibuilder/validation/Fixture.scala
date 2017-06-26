package io.apibuilder.validation

import java.io.File
import java.net.URLEncoder

import play.api.libs.json.{JsObject, JsValue, Json}

case class Fixture(params: Seq[(String, String)], expected: JsObject) {

  def rawQueryString: String = {
    params.map { case (k,v) => s"$k=" + URLEncoder.encode(v, "UTF-8") }.mkString("&")
  }

}

object Fixture {

  private[this] val CommentCharacter = "#"

  def load(file: File): Fixture = {
    scala.io.Source.fromFile(file).getLines.mkString("\n").
      split("\n").map(_.trim).filter { l => !l.startsWith(CommentCharacter) }.mkString("\n").
      trim.split("\n\n").toList match {
      case definition :: expected :: Nil => {
        Fixture(
          params = parseParameters(file, definition),
          expected = Json.parse(expected).as[JsObject]
        )
      }
      case _ => sys.error(s"File[$file] Could not parse contents - no newline found")
    }
  }

  private[this] def parseParameters(file: File, value: String): Seq[(String, String)] = {
    value.split("\n").map(_.trim).filter(_.nonEmpty).map { v =>
      v.split("=").toList match {
        case k :: Nil => (k, "")
        case k :: v :: Nil => (k, v)
        case _ => sys.error(s"File[$file] Could not parse parameter declaration: $v")
      }
    }
  }

}