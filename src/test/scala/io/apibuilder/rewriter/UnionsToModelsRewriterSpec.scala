package io.apibuilder.rewriter

import io.apibuilder.helpers.{ApiBuilderServiceHelpers, MultiServiceHelpers}
import io.apibuilder.spec.v0.models.{Model, Union}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class UnionsToModelsRewriterSpec extends AnyWordSpec with Matchers
  with ApiBuilderServiceHelpers
  with MultiServiceHelpers {

  private[this] def rewrite(models: Seq[Model], unions: Seq[Union]) = {
    UnionsToModelsRewriter.rewrite(
      makeMultiService(
        makeService(models = models, unions = unions)
      )
    )
  }

  private[this] def union(name: String, discriminator: String, types: Seq[String]): Union = {
    makeUnion(
      name,
      types = types.map { t => makeUnionType(t) },
      discriminator = Some(discriminator),
    )
  }

  "rewrites union to a model and enum" in {
    val models = Seq(makeModel("user"), makeModel("group"))
    val ms = rewrite(
      models = models,
      unions = Seq(union("party", "disc", models.map(_.name))),
    )

    findUnion(ms, "party") must be(None)
    models.forall { m => findModel(ms, m.name).isDefined } must be(true)

    val party = mustFindModel(ms, "party").model
    party.fields.map(_.name) must equal(Seq("disc"))
    mustFindEnum(ms, "party_disc").`enum`.values.map(_.name) must equal(
      Seq("user", "group")
    )
  }
}
