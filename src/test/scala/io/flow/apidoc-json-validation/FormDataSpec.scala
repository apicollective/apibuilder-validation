package io.flow.lib.apidoc.json.validation

import org.scalatest.{Matchers, FunSpec}
import play.api.libs.json._

class FormDataSpec extends FunSpec with Matchers {

  it("parseEncoded") {
    FormData.parseEncoded("") should be(
      Map()
    )

    FormData.parseEncoded("key=val1&key=val2") should be(
      Map("key" -> Seq("val1", "val2"))
    )
    
    FormData.parseEncoded("key=val1&key=val2&foo=bar") should be(
      Map(
        "key" -> Seq("val1", "val2"),
        "foo" -> Seq("bar")
      )
    )
  }

  describe("toJson") {

    val data: Map[String, Seq[String]] = Map(
      "email" -> Seq("test@flow.io"),
      "name[first]" -> Seq("mike"),
      "name[last]" -> Seq("roth"),
      "one[two][three][four]" -> Seq("haha"),
      "one[two][three][five]" -> Seq("wow"),
      "arr[][arr2][]" -> Seq("fruit", "vegitables"),
      "tags[]" -> Seq("foo", "bar"),
      "yikes" -> Seq("yes", "no")
    )

    it("returns JsValue") {
      FormData.toJson(data) match {
        case res: JsValue => assert(true)
        case _ => assert(false)
      }
    }

    it("creates simple json object") {
      (FormData.toJson(data) \ "email").validate[String] match {
        case JsSuccess(succ,_) => succ should be("test@flow.io")
        case JsError(_) => assert(false)
      }
    }

    it("creates complex json object") {
      (FormData.toJson(data) \ "name" \ "first").validate[String] match {
        case JsSuccess(succ,_) => succ should be("mike")
        case JsError(_) => assert(false)
      }

      (FormData.toJson(data) \ "name" \ "last").validate[String] match {
        case JsSuccess(succ,_) => succ should be("roth")
        case JsError(_) => assert(false)
      }
    }

    it("creates simple array json object") {
      (FormData.toJson(data) \ "tags").validate[Seq[String]] match {
        case JsSuccess(succ,_) => succ should be(Seq("foo", "bar"))
        case JsError(_) => assert(false)
      }
    }

    it("creates complex array json object") {
      (FormData.toJson(data) \ "arr").validate[Seq[JsValue]] match {
        case JsSuccess(succ,_) => assert((succ.head \ "arr2").toOption.isDefined)
        case JsError(_) => assert(false)
      }
    }

    it("takes first instance of non-array key") {
      (FormData.toJson(data) \ "yikes").validate[String] match {
        case JsSuccess(succ,_) => succ should be("yes")
        case JsError(_) => assert(false)
      }
    }
  }
}
