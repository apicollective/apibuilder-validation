package io.apibuilder.validation

import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec

class TypeNameSpec extends AnyFunSpec with Matchers {

  it("flat type name") {
    TypeName.parse("person", defaultNamespace = "io.flow") should equal(TypeName("person", "io.flow"))
    TypeName.parse("foo", defaultNamespace = "io.flow") should equal(TypeName("foo", "io.flow"))
  }

  it("type name with namespace  ") {
    TypeName.parse(
      "io.apibuilder.explicit.validation.core.v0.enums.gender",
      defaultNamespace = "foo"
    ) should equal(
      TypeName("gender", "io.apibuilder.explicit.validation.core.v0")
    )
  }

  it("type name without namespace  ") {
    TypeName.parse("foo") should be(None)

    TypeName.parse(
      "io.apibuilder.explicit.validation.core.v0.enums.gender"
    ) should equal(
      Some(TypeName("gender", "io.apibuilder.explicit.validation.core.v0"))
    )
  }

}
