package io.apibuilder.validation

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TypeNameSpec extends AnyWordSpec with Matchers {

  "flat type name" in {
    TypeName.parse("person", defaultNamespace = "io.flow") must equal(TypeName("person", "io.flow"))
    TypeName.parse("foo", defaultNamespace = "io.flow") must equal(TypeName("foo", "io.flow"))
  }

  "type name with namespace" in {
    TypeName.parse(
      "io.apibuilder.explicit.validation.core.v0.enums.gender",
      defaultNamespace = "foo"
    ) must equal(
      TypeName("gender", "io.apibuilder.explicit.validation.core.v0")
    )
  }

  "type name without namespace" in {
    TypeName.parse("foo") must be(None)

    TypeName.parse(
      "io.apibuilder.explicit.validation.core.v0.enums.gender"
    ) must equal(
      Some(TypeName("gender", "io.apibuilder.explicit.validation.core.v0"))
    )
  }

}
