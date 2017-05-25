package io.flow.lib.apidoc.json.validation

import java.io.File

import org.scalatest.{FunSpec, Matchers}
import play.api.libs.json.Json

class QueryStringSpec extends FunSpec with Matchers {

  private[this] val Dir: File = {
    val d = new File("src/test/resources/querystring")
    assert(d.exists(), s"Dir[$d] does not exist")
    d
  }

  it("examples") {
    val files = Dir.listFiles.filter(_.getName.endsWith(".fixture"))
    files.nonEmpty should be(true)
    files.foreach { file =>
      val fixture = Fixture.load(file)
      val parsed = FormData.parseEncodedToJsObject(fixture.rawQueryString)

      if (parsed != fixture.expected) {
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

        sys.error(s"$Dir/${file.getName}: ${fixture.rawQueryString} - JsValue did not match expected")
      }
    }
  }
}

