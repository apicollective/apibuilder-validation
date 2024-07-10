package io.apibuilder.validation

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json._

class FormDataSpec extends AnyWordSpec with Matchers {

  "toInteger" in {
    FormData.toLong("") must be(None)
    FormData.toLong("a") must be(None)
    FormData.toLong("1") must be(Some(1))
    FormData.toLong("-1") must be(Some(-1))
    FormData.toLong("1999") must be(Some(1999))
    FormData.toLong("-1999") must be(Some(-1999))
    FormData.toLong("0101") must be(None)
    FormData.toLong("0101.") must be(None)
    FormData.toLong("0101.00") must be(None)
    FormData.toLong("0101.10") must be(None)
    FormData.toLong("-0101.00") must be(None)
    FormData.toLong("-0101.10") must be(None)
    FormData.toLong("1999.10") must be(None)
    FormData.toLong("1999.00") must be(None)
    FormData.toLong("-1999.10") must be(None)
    FormData.toLong("-1999.00") must be(None)
  }

  "parseEncoded" in {
    FormData.parseEncoded("") must be(
      Map()
    )

    FormData.parseEncoded("key=val1&key=val2") must be(
      Map("key" -> Seq("val1", "val2"))
    )

    FormData.parseEncoded("key=val1&key=val2&foo=bar") must be(
      Map(
        "key" -> Seq("val1", "val2"),
        "foo" -> Seq("bar")
      )
    )

    FormData.parseEncoded("q=category:shoes") must be(
      Map(
        "q" -> Seq("category:shoes")
      )
    )
  }

  "rewriteEncoded" in {
    FormData.rewriteEncoded("number[]=100379876543&number[]=WT65xSPLX-SPT-5") must be(
      "number[0]=100379876543&number[1]=WT65xSPLX-SPT-5"
    )

    FormData.rewriteEncoded("user[name][first]=mike&user[name][last]=bryzek") must be(
      "user[name][first]=mike&user[name][last]=bryzek"
    )

    FormData.rewriteEncoded("user[name][first][]=mike&user[name][first][]=maciej&user[name][last]=bryzek") must be(
      "user[name][first][0]=mike&user[name][first][1]=maciej&user[name][last]=bryzek"
    )

    FormData.rewriteEncoded("q=category:shoes") must be(
      "q=category%3Ashoes"
    )
  }

  "arrays with []" in {
    FormData.parseEncoded("number[]=1&number[]=2") must be(
      Map("number[]" -> Seq("1", "2"))
    )

    FormData.toJson(FormData.parseEncoded("number[]=1&number[]=2")) must be(
      Json.obj("number" -> Seq(1, 2))
    )
  }

  "nested arrays" in {
    FormData.parseEncoded("a[0][b[0]][c]=d") must be(
      Map("a[0][b[0]][c]" -> Seq("d"))
    )
  }

  "arrays" in {
    FormData.parseEncoded("key=val1&key=val2") must be(
      Map("key" -> Seq("val1", "val2"))
    )

    FormData.parseEncoded("key=val1&key=val2&foo=bar") must be(
      Map(
        "key" -> Seq("val1", "val2"),
        "foo" -> Seq("bar")
      )
    )

    // Now test with 'key' in different order
    FormData.parseEncoded("key=val1&foo=bar&key=val2") must be(
      Map(
        "key" -> Seq("val1", "val2"),
        "foo" -> Seq("bar")
      )
    )
  }

  "toJson parses multiple values" in {
    val data = Map("foo" -> Seq("a", "b"), "foo2" -> Seq("c"))
    val js = FormData.toJson(data)

    val foo: Seq[String] = (js \ "foo").as[JsArray].value.map(_.asInstanceOf[JsString].value).toSeq
    foo must equal(Seq("a", "b"))

    val foo2: String = (js \ "foo2").as[JsString].value
    foo2 must equal("c")
  }

  "toJson parses single value" in {
    val data = Map("foo" -> "a", "bar" ->"b")
    FormData.toJsonFromSimpleMap(data) must equal(
      Json.obj(
        "foo" -> "a",
        "bar" -> "b"
      )
    )
  }

  "toJson" must {

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

    "returns JsValue" in {
      FormData.toJson(data) match {
        case _: JsValue => assert(true)
        case _ => assert(false)
      }
    }

    "creates simple json object" in {
      (FormData.toJson(data) \ "email").validate[String] match {
        case JsSuccess(succ, _) => succ must be("test@flow.io")
        case JsError(_) => assert(false)
      }
    }

    "creates complex json object" in {
      (FormData.toJson(data) \ "name" \ "first").validate[String] match {
        case JsSuccess(succ, _) => succ must be("mike")
        case JsError(_) => assert(false)
      }

      (FormData.toJson(data) \ "name" \ "last").validate[String] match {
        case JsSuccess(succ, _) => succ must be("roth")
        case JsError(_) => assert(false)
      }
    }

    "creates simple array json object" in {
      (FormData.toJson(data) \ "tags").validate[Seq[String]] match {
        case JsSuccess(succ, _) => succ must be(Seq("foo", "bar"))
        case JsError(_) => assert(false)
      }
    }

    "creates complex array json object" in {
      (FormData.toJson(data) \ "arr").validate[Seq[JsValue]] match {
        case JsSuccess(succ, _) => assert((succ.head \ "arr2").toOption.isDefined)
        case JsError(_) => assert(false)
      }
    }

    "multi valued arrays are lists" in {
      (FormData.toJson(data) \ "yikes").validate[Seq[String]] match {
        case JsSuccess(succ, _) => succ must be(Seq("yes", "no"))
        case JsError(_) => assert(false)
      }
    }

    "handles empty strings" in {
      val res = FormData.toJson(data).fields.find(_._1 == "anEmptyString")
      res must be(Some("anEmptyString" -> JsNull))
    }

    "preserve leading zeroes" in {
      FormData.normalize(
        Seq(
          ("number1", "0100"),
          ("number1", "0100."),
          ("number2", "0100.10")
        ),
        options = Set(EncodingOptions.OmitArrayIndexes)
      ) must equal(
        Seq(
          ("number1", "0100"),
          ("number1", "0100."),
          ("number2", "0100.10")
        )
      )
    }

    "toJson for seq of tuples handles array bracket format" in {
      FormData.normalize(
        Seq(
          ("limit", "100"),
          ("variant_id[1]", "foo"),
          ("variant_id[0]", "bar")
        ),
        options = Set(EncodingOptions.OmitArrayIndexes)
      ) must equal(
        Seq(
          ("limit", "100"),
          ("variant_id", "bar"),
          ("variant_id", "foo")
        )
      )
    }

    "toJson for seq of tuples defaults to including array index" in {
      FormData.normalize(
        Seq(
          ("limit", "100"),
          ("variant_id[1]", "foo"),
          ("variant_id[0]", "bar")
        )
      ) must equal(
        Seq(
          ("limit", "100"),
          ("variant_id[0]", "bar"),
          ("variant_id[1]", "foo")
        )
      )
    }

    "normalize does not wipe out multi params with no brackets" in {
      FormData.normalize(
        Seq(
          ("limit", "100"),
          ("variant_id", "foo"),
          ("variant_id", "bar")
        ),
        options = Set(EncodingOptions.OmitArrayIndexes)
      ) must equal(
        Seq(
          ("limit", "100"),
          ("variant_id", "foo"),
          ("variant_id", "bar")
        )
      )
    }
  }
}
