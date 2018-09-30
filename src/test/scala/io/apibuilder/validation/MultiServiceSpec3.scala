package io.apibuilder.validation

import org.scalatest.{FunSpec, Matchers}

class MultiServiceSpec3 extends FunSpec with Matchers with helpers.Helpers {

  it("correctly parses required fields") {
    val orgModel = flowMultiService.findType("organization").get.asInstanceOf[ApibuilderType.Model].model
    orgModel.fields.find(_.name == "id").get.required should be(true)
    orgModel.fields.find(_.name == "parent").get.required should be(false)
  }
}
