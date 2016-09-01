package io.flow.lib.apidoc.json.validation

import org.scalatest.{FunSpec, Matchers}

class SampleSpec extends FunSpec with Matchers {

  it("test") {
    val contents = scala.io.Source.fromFile("src/test/resources/apidoc-api-service.json", "UTF-8").getLines.mkString("\n")
    1 should be(1)
  }

}
