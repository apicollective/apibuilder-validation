package io.flow.lib.apidoc.json.validation

import java.io.File

import org.scalatest.{FunSpec, Matchers}

/**
  * Parse query strings into validated JSON objects
  */
class PaypalOrderConversionsSpec extends FunSpec with Matchers {

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
      println(s"${file.getName}: $fixture")
    }
  }
}

