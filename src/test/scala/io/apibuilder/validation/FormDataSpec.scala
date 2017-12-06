package io.apibuilder.validation

import java.io.File

import org.scalatest.{FunSpec, Matchers}
import play.api.libs.json._

class FormDataSpec extends FunSpec with Matchers {

  it("toNumber") {
    FormData.toNumber("") should be(None)
    FormData.toNumber("a") should be(None)
    FormData.toNumber("1") should be(Some(1))
    FormData.toNumber("-1") should be(Some(-1))
    FormData.toNumber("1999") should be(Some(1999))
    FormData.toNumber("-1999") should be(Some(-1999))
  }

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

    FormData.parseEncoded("q=category:shoes") should be(
      Map(
        "q" -> Seq("category:shoes")
      )
    )
  }

  it("rewriteEncoded") {
    FormData.rewriteEncoded("number[]=100379876543&number[]=WT65xSPLX-SPT-5") should be(
      "number[0]=100379876543&number[1]=WT65xSPLX-SPT-5"
    )

    FormData.rewriteEncoded("user[name][first]=mike&user[name][last]=bryzek") should be(
      "user[name][first]=mike&user[name][last]=bryzek"
    )

    FormData.rewriteEncoded("user[name][first][]=mike&user[name][first][]=maciej&user[name][last]=bryzek") should be(
      "user[name][first][0]=mike&user[name][first][1]=maciej&user[name][last]=bryzek"
    )

    FormData.rewriteEncoded("q=category:shoes") should be(
      "q=category%3Ashoes"
    )
  }

  it("arrays with []") {
    FormData.parseEncoded("number[]=1&number[]=2") should be(
      Map("number[]" -> Seq("1", "2"))
    )

    FormData.toJson(FormData.parseEncoded("number[]=1&number[]=2")) should be(
      Json.obj("number" -> Seq(1, 2))
    )
  }

  it("nested arrays") {
    FormData.parseEncoded("a[0][b[0]][c]=d") should be(
      Map("a[0][b[0]][c]" -> Seq("d"))
    )
  }

  it("arrays") {
    FormData.parseEncoded("key=val1&key=val2") should be(
      Map("key" -> Seq("val1", "val2"))
    )

    FormData.parseEncoded("key=val1&key=val2&foo=bar") should be(
      Map(
        "key" -> Seq("val1", "val2"),
        "foo" -> Seq("bar")
      )
    )

    // Now test with 'key' in different order
    FormData.parseEncoded("key=val1&foo=bar&key=val2") should be(
      Map(
        "key" -> Seq("val1", "val2"),
        "foo" -> Seq("bar")
      )
    )
  }

  it("toJson parses multiple values") {
    val data = Map("foo" -> Seq("a", "b"), "foo2" -> Seq("c"))
    val js = FormData.toJson(data)

    val foo: Seq[String] = (js \ "foo").as[JsArray].value.map(_.asInstanceOf[JsString].value)
    foo should equal(Seq("a", "b"))

    val foo2: String = (js \ "foo2").as[JsString].value
    foo2 should equal("c")
  }

  describe("toJson") {

    val data: Map[String, Seq[String]] = Map(
      "email" -> Seq("test@flow.io"),
      "name[first]" -> Seq("mike"),
      "name[last]" -> Seq("roth"),
      "one[two][three][four]" -> Seq("haha"),
      "one[two][three][five]" -> Seq("wow"),
      "arr[][arr2][]" -> Seq("fruit", "vegetables"),
      "tags[]" -> Seq("foo", "bar"),
      "yikes" -> Seq("yes", "no"),
      "anEmptyString" -> Seq(null)
    )

    it("returns JsValue") {
      FormData.toJson(data) match {
        case res: JsValue => assert(true)
        case _ => assert(false)
      }
    }

    it("creates simple json object") {
      (FormData.toJson(data) \ "email").validate[String] match {
        case JsSuccess(succ, _) => succ should be("test@flow.io")
        case JsError(_) => assert(false)
      }
    }

    it("creates complex json object") {
      (FormData.toJson(data) \ "name" \ "first").validate[String] match {
        case JsSuccess(succ, _) => succ should be("mike")
        case JsError(_) => assert(false)
      }

      (FormData.toJson(data) \ "name" \ "last").validate[String] match {
        case JsSuccess(succ, _) => succ should be("roth")
        case JsError(_) => assert(false)
      }
    }

    it("creates simple array json object") {
      (FormData.toJson(data) \ "tags").validate[Seq[String]] match {
        case JsSuccess(succ, _) => succ should be(Seq("foo", "bar"))
        case JsError(_) => assert(false)
      }
    }

    it("creates complex array json object") {
      (FormData.toJson(data) \ "arr").validate[Seq[JsValue]] match {
        case JsSuccess(succ, _) => assert((succ.head \ "arr2").toOption.isDefined)
        case JsError(_) => assert(false)
      }
    }

    it("multi valued arrays are lists") {
      (FormData.toJson(data) \ "yikes").validate[Seq[String]] match {
        case JsSuccess(succ, _) => succ should be(Seq("yes", "no"))
        case JsError(_) => assert(false)
      }
    }

    it("handles empty strings") {
      val res = FormData.toJson(data).fields.find(_._1 == "anEmptyString")
      res should be(Some("anEmptyString" -> JsNull))
    }

  }
}
