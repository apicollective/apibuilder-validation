package io.apibuilder.validation

import java.io.File
import org.scalatest.{FunSpec, Matchers}
import play.api.libs.json.{JsArray, JsObject, JsValue, JsNull, Json}

class QueryStringSpec extends FunSpec with Matchers {

  private[this] val Dir: File = {
    val d = new File("src/test/resources/querystring")
    assert(d.exists(), s"Dir[$d] does not exist")
    d
  }

  /**
    * Recursively diff the two objects to highlight specific field errors
    */
  private[this] def diff(a: JsValue, b: JsValue, differences: Seq[String] = Nil, desc: Option[String] = None): Seq[String] = {
    (a, b) match {
      case (a: JsObject, b: JsObject) => {
        if (a.keys == b.keys) {
          differences ++ a.keys.flatMap { k =>
            diff(
              (a \ k).as[JsValue],
              (b \ k).as[JsValue],
              differences,
              Some(desc.map { d => s"$d[$k]" }.getOrElse(k))
            )
          }
        } else {
          val missing = a.keys.diff(b.keys)
          val additional = b.keys.diff(a.keys)
          differences ++ differences ++ Seq(s"${desc.getOrElse("")}: Missing keys[$missing]. Additional keys[$additional]")
        }
      }

      case (_: JsObject, _) => {
        differences ++ differences ++ Seq(s"${desc.getOrElse("")}: Expected Object but found ${b.getClass.getName}")
      }

      case (a: JsArray, b: JsArray) => {
        if (a.value.length != b.value.length) {
          differences ++ Seq(s"${desc.getOrElse("")}: Expected array to be of length[${a.value.length}] but found[${b.value.length}]")
        } else {
          differences ++ a.value.zipWithIndex.flatMap { case (v, i) =>
            diff(
              v,
              b.value(i),
              differences,
              Some(desc.map { d => s"$d[$i]" }.getOrElse(i.toString))
            )
          }
        }
      }

      case (_: JsArray, _) => {
        differences ++ Seq(s"${desc.getOrElse("")}: Expected Array but found ${b.getClass.getName}")
      }

      case (_, _) if a == b => differences

      case (_, _) if a.toString() == "null" && b.toString() == "null" => differences // for our purposes, null is equivalent

      case (_, _) => differences ++ Seq(s"${desc.getOrElse("")}: Expected[$a] but found[$b]")
    }
  }

  it("examples") {
    val files = Dir.listFiles.filter(_.getName.endsWith(".fixture"))
    files.nonEmpty should be(true)
    files.foreach { file =>
      val fixture = Fixture.load(file)
      val parsed = FormData.parseEncodedToJsObject(fixture.rawQueryString)

      val differences = diff(fixture.expected, parsed)
      if (differences.nonEmpty) {
        println("")
        println("parseEncoded")
        println("----------------------------------------")
        FormData.parseEncoded(fixture.rawQueryString).foreach { case (k, values) =>
          println(s" - $k: ${values}")
        }

        println("")
        println("EXPECTED")
        println("----------------------------------------")
        println(Json.prettyPrint(fixture.expected))

        println("")
        println("PARSED")
        println("----------------------------------------")
        println(Json.prettyPrint(parsed))

        println("")
        println("Differences")
        println("----------------------------------------")
        differences.foreach { d =>
          println(s" - $d")
        }

        sys.error(s"$Dir/${file.getName}: ${fixture.rawQueryString} - JsValue did not match expected")
      }
    }
  }
}

