package io.apibuilder.validation

import io.apibuilder.builders.ApiBuilderServiceBuilders
import io.apibuilder.spec.v0.models.{Enum, Model, Union}
import io.apibuilder.validation.helpers.Helpers
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ApiBuilderTypeSpec extends AnyWordSpec with Matchers with Helpers with ApiBuilderServiceBuilders {

  private lazy val service = ApiBuilderService(makeService(
    enums = Seq(makeEnum("gender")),
    models = Seq(makeModel("user")),
    unions = Seq(makeUnion("test")),
  ))
  private lazy val `enum` = service.enums.head
  private lazy val model = service.models.head
  private lazy val union = service.unions.head

  "namespace" in {
    `enum`.namespace must equal(service.namespace)
    model.namespace must equal(service.namespace)
    union.namespace must equal(service.namespace)
  }

  "name" in {
    `enum`.name must equal("gender")
    model.name must equal("user")
    union.name must equal("test")
  }

  "qualified" in {
    `enum`.qualified must equal(service.namespace + ".enums.gender")
    model.qualified must equal(service.namespace + ".models.user")
    union.qualified must equal(service.namespace + ".unions.test")
  }

  "qualified is compatible with Type Name" in {
    TypeName.parse(defaultNamespace = "foo", name = `enum`.qualified) must equal(
      TypeName(name = "gender", namespace = service.namespace)
    )
  }

}
