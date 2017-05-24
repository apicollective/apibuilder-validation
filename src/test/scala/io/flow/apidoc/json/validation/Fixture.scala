package io.flow.lib.apidoc.json.validation

import java.io.File
import play.api.libs.json.{JsObject, JsValue, Json}

case class Fixture(params: Seq[(String, String)], expected: JsValue)

object Fixture {

  def load(file: File): Fixture = {
    val contents = scala.io.Source.fromFile(file).getLines.mkString("")
    contents.split("\n\n").toList match {
      case definition :: expected :: Nil => {
        Fixture(
          params = parseParameters(file, definition),
          expected = Json.parse(expected).as[JsObject]
        )
      }
      case _ => sys.error(s"File[$file] Could not parse contents - no newline found")
    }
  }

  def parseParameters(file: File, value: String): Seq[(String, String)] = {
    value.split("\n").map(_.trim).filter(_.nonEmpty).map { v =>
      v.split("=").toList match {
        case k :: v :: Nil => (k, v)
        case _ => sys.error(s"File[$file] Could not parse parameter declaration: $v")
      }
    }
    Nil
  }

}