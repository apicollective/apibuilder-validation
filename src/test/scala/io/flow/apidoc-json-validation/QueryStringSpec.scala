package io.flow.lib.apidoc.json.validation

import java.io.File

import org.scalatest.{FunSpec, Matchers}

/**
  * Parse query strings into validated JSON objects
  */
class PaypalOrderConversionsSpec extends FunSpec with Matchers {

  private[this] val Dir: File = {
    val d = new File("test/resources/querystring")
    assert(d.exists(), s"Dir[$d] does not exist")
    d
  }

  it(examples") {
    for ( file <- Dir.listFiles if file.getName.endsWith(".fixutre") ) {
      val fixture = Fixture.load(file)
      println(s"Fixture: $fixture")
    }
  }
}

