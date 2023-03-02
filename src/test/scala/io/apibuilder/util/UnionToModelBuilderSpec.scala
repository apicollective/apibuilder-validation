package io.apibuilder.util

import cats.data.Validated.{Invalid, Valid}
import io.apibuilder.builders.{ApiBuilderServiceBuilders, MultiServiceBuilders}
import io.apibuilder.spec.v0.models._
import io.apibuilder.validation.{ApiBuilderService, ApiBuilderType}
import org.scalatest.Assertion
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class UnionToModelBuilderSpec extends AnyWordSpec with Matchers
  with ApiBuilderServiceBuilders
  with MultiServiceBuilders
{
  private[this] def setup(
    discriminator: Option[String] = Some("discriminator"),
    enums: Seq[Enum],
    models: Seq[Model],
    unions: Seq[Union],
    defaultType: Option[String] = None,
  ) = {
    val union = makeUnion(
      name = "example_union",
      discriminator = discriminator,
      types = (enums.map(_.name) ++ models.map(_.name) ++ unions.map(_.name)).map { name =>
        makeUnionType(name, default = Some(defaultType.contains(name)))
      }
    )
    val service = makeService(
      namespace = "test",
      enums = enums,
      models = models,
      unions = unions ++ Seq(union),
    )
    UnionToModelBuilder(makeMultiService(service)).toModel(ApiBuilderType.Union(ApiBuilderService(service), union))
  }

  private[this] def setupValid(
    enums: Seq[Enum] = Nil,
    models: Seq[Model],
    unions: Seq[Union] = Nil,
    defaultType: Option[String] = None,
  ): UnionModel = {
    setup(
      enums = enums,
      models = models,
      unions = unions,
      defaultType = defaultType,
    ) match {
      case Valid(m) => m
      case Invalid(errors) => sys.error(s"Invalid: ${errors.toNonEmptyList.toList}")
    }
  }

  private[this] def setupInvalid(
    discriminator: Option[String] = Some("discriminator"),
    enums: Seq[Enum] = Nil,
    models: Seq[Model] = Nil,
    unions: Seq[Union] = Nil,
  ): Seq[String] = {
    setup(
      discriminator = discriminator,
      enums = enums,
      models = models,
      unions = unions,
    ) match {
      case Valid(_) => sys.error("Expected invalid")
      case Invalid(errors) => errors.toNonEmptyList.toList
    }
  }

  "validates discriminator" in {
    setupInvalid(discriminator = None) must equal(
      Seq("Union 'test.unions.example_union' must have a 'discriminator' defined'")
    )
  }

  "validates all types are models" must {
    "enum" in {
      val `enum` = makeEnum("test")
      setupInvalid(enums = Seq(enum)) must equal(
        Seq("Type 'test' is an enum. A model is required")
      )
    }

    "union" in {
      val union = makeUnion("test")
      setupInvalid(unions = Seq(union)) must equal(
        Seq("Type 'test' is a union. A model is required")
      )
    }
  }

  "validates fields types" must {
    def setupModels(a: String, b: String) = {
      val user1 = makeModel("user1", fields = Seq(makeField("id", a)))
      val user2 = makeModel("user2", fields = Seq(makeField("id", b)))
      setupValid(models = Seq(user1, user2))
    }
    def setupFieldTypes(a: String, b: String) = {
      setupModels(a, b).model.fields.find(_.name == "id").get
    }

    "when incompatible" in {
      def model(typ: String) = makeModel(fields = Seq(makeField("id", typ)))
      setupInvalid(models = Seq(
        model("string"),
        model("foo"),
        model("long"),
      )) mustEqual(
        Seq("Union type 'test.unions.example_union' Field 'id' has incompatible types: foo, long, string - all union types must have a common type")
      )
    }

    "upcast to string if necessary and all types are scalars" in {
      setupFieldTypes("string", "long").`type` must equal("string")
      setupFieldTypes("string", "boolean").`type` must equal("string")
    }

    "preserves type if possible" in {
      setupFieldTypes("long", "long").`type` must equal("long")
    }
  }

  "valid" must {
    "single model" in {
      val model = makeModel("user", fields = Seq(makeField("id", "long")))
      val m = setupValid(models = Seq(model))
      m.model.fields.map(_.name) must equal(
        Seq("discriminator", "id")
      )
      m.model.fields.map(_.`type`) must equal(
        Seq("example_union_discriminator", "long")
      )
      m.discriminatorEnum.name must equal("example_union_discriminator")
      m.discriminatorEnum.values.map(_.name) must equal(
        Seq("user")
      )
      m.discriminatorEnum.values.map(_.value) must equal(
        Seq(None)
      )
    }

    "discriminator required" must {
      def setupDiscRequired(defaultType: Option[String]) = {
        val m = setupValid(models = Seq(makeModel("user")), defaultType = defaultType)
        m.model.fields.find(_.name == "discriminator").get.required
      }

      "is true when there is no default type" in {
        setupDiscRequired(None) must be(true)
      }

      "is false when there is a default type" in {
        setupDiscRequired(Some("user")) must be(false)
      }
    }

    "default" must {
      def setupDefault(a: Option[String], b: Option[String]) = {
        val user1 = makeModel("user1", fields = Seq(makeField("id", "long", default = a)))
        val user2 = makeModel("user2", fields = Seq(makeField("id", "long", default = b)))
        setupValid(models = Seq(user1, user2)).model.fields.find(_.name == "id").get.default
      }

      "matches" in {
        setupDefault(Some("1"), Some("1")) must equal(Some("1"))
      }

      "different" in {
        setupDefault(Some("1"), Some("2")) must equal(None)
      }

      "not specified" in {
        setupDefault(Some("1"), None) must equal(None)
      }
    }

    "required" must {
      def setupRequired(a: Boolean, b: Boolean) = {
        val user1 = makeModel("user1", fields = Seq(makeField("id", "long", required = a)))
        val user2 = makeModel("user2", fields = Seq(makeField("id", "long", required = b)))
        setupValid(models = Seq(user1, user2)).model.fields.find(_.name == "id").get.required
      }

      "true if all models mark the field required" in {
        setupRequired(true, true) must equal(true)
      }

      "false if any model has the field as optional" in {
        setupRequired(false, true) must equal(false)
        setupRequired(true, false) must equal(false)
        setupRequired(false, false) must equal(false)
      }
    }
  }

  "multiple models" must {
    "will merge fields by name across models" in {
      val user = makeModel("user", fields = Seq(
        makeField("id", "long"),
        makeField("email"),
        makeField("other", default = Some("foo")),
      ))
      val group = makeModel("group", fields = Seq(
        makeField("id", "long"),
        makeField("name"),
        makeField("other", default = Some("foo")),
      ))
      val result = setupValid(models = Seq(user, group))
      result.model.fields.map(_.name) must equal(
        Seq("discriminator", "id", "email", "other", "name")
      )

      def fld(name: String)(f: Field => Assertion) = {
        f(result.model.fields.find(_.name == name).get)
      }

      fld("id") { f =>
        f.`type` must be("long")
        f.required must be(true)
        f.default must be(None)
      }

      fld("email") { f =>
        f.`type` must be("string")
        f.required must be(false)
        f.default must be(None)
      }

      fld("name") { f =>
        f.`type` must be("string")
        f.required must be(false)
        f.default must be(None)
      }

      fld("other") { f =>
        f.`type` must be("string")
        f.required must be(true)
        f.default must be(Some("foo"))
      }
    }
  }

}
