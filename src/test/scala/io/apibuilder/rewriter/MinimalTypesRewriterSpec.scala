package io.apibuilder.rewriter

import io.apibuilder.builders.{ApiBuilderServiceBuilders, MultiServiceBuilders}
import io.apibuilder.spec.v0.models.{Enum, Model, Union}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MinimalTypesRewriterSpec extends AnyWordSpec with Matchers
  with ApiBuilderServiceBuilders
  with MultiServiceBuilders {

  private[this] def rewrite(
    enums: Seq[Enum] = Nil,
    models: Seq[Model] = Nil,
    unions: Seq[Union] = Nil,
    types: Seq[String],
  ) = {
    val ms = makeMultiService(
      makeService(enums = enums, models = models, unions = unions)
    )
    MinimalTypesRewriter(
      types.map { t => mustFindApiBuilderType(ms, t) }
    ).rewrite(ms).allTypes.map(_.name).sorted
  }

  "model fields" must {

    "resolve enum type" in {
      rewrite(
        enums = Seq(makeEnum("foo")),
        models = Seq(
          makeModel("user", fields = Seq(makeField(`type` = "foo")))
        ),
        types = Seq("user")
      ) must equal(Seq("foo", "user"))
    }

    "resolve model type" in {
      rewrite(
        models = Seq(
          makeModel("foo"),
          makeModel("user", fields = Seq(makeField(`type` = "foo")))
        ),
        types = Seq("user")
      ) must equal(Seq("foo", "user"))
    }

    "resolve union type" in {
      rewrite(
        unions = Seq(makeUnion("foo")),
        models = Seq(
          makeModel("user", fields = Seq(makeField(`type` = "foo")))
        ),
        types = Seq("user")
      ) must equal(Seq("foo", "user"))
    }

  }

  "unions types" must {

    "resolve enum type" in {
      rewrite(
        enums = Seq(makeEnum("foo")),
        unions = Seq(
          makeUnion("user", types = Seq(makeUnionType(`type` = "foo")))
        ),
        types = Seq("user")
      ) must equal(Seq("foo", "user"))
    }

    "resolve model type" in {
      rewrite(
        models = Seq(makeModel("foo")),
        unions = Seq(
          makeUnion("user", types = Seq(makeUnionType(`type` = "foo")))
        ),
        types = Seq("user")
      ) must equal(Seq("foo", "user"))
    }

    "resolve union type" in {
      rewrite(
        unions = Seq(
          makeUnion("foo"),
          makeUnion("user", types = Seq(makeUnionType(`type` = "foo")))
        ),
        types = Seq("user")
      ) must equal(Seq("foo", "user"))
    }

  }

}
