package io.apibuilder.validation

import org.scalatest.{FunSpec, Matchers}

class TypeNameSpec extends FunSpec with Matchers {

  it("flat type name") {
    TypeName("person") should equal(TypeName("person", None))
    TypeName("foo") should equal(TypeName("foo", None))
  }

  it("type name with namespace  ") {
    TypeName("io.apibuilder.explicit.validation.core.v0.enums.gender") should equal(
      TypeName("gender", Some("io.apibuilder.explicit.validation.core"))
    )
  }

}
