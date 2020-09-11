package io.apibuilder.rewriter

import io.apibuilder.helpers.{ApiBuilderServiceHelpers, ApiBuilderServiceValidatorHelpers, MultiServiceHelpers}
import io.apibuilder.spec.v0.models.Service
import io.apibuilder.validation.ApiBuilderService
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class UnionTypesMustBeModelsRewriterSpec extends AnyWordSpec with Matchers
  with ApiBuilderServiceHelpers
  with MultiServiceHelpers
  with ApiBuilderServiceValidatorHelpers
{

  private[this] def rewrite(service: Service): ApiBuilderService = {
    val ms = makeMultiService(ApiBuilderService(service))
    apiBuilderValidator.mustValidateDefaultProfile(ms)
    UnionTypesMustBeModelsRewriter.rewrite(ms).services().head
  }

  "replaces unions with an enum type" in {
    val group = makeValidEnum("group")
    val union = makeUnion("union", types = Seq(makeUnionType(group.name)))
    val person = makeModel("person", fields = Seq(
      makeField("id"), makeField(`type` = union.name)
    ))

    val service = rewrite(
      makeService(
        enums = Seq(group),
        models = Seq(person),
        unions = Seq(union),
      )
    )

    findUnion(service, union.name) must be(None)
    mustFindModel(service, person.name).model.fields.map(_.`type`) must equal(Seq("string", "object"))
  }

  "keeps unions with all models types" in {
    val group = makeModel("group")
    val union = makeUnion("union", types = Seq(makeUnionType(group.name)))

    val service = rewrite(
      makeService(
        models = Seq(group),
        unions = Seq(union),
      )
    )

    mustFindUnion(service, union.name).union.types.map(_.`type`) must equal(Seq("group"))
  }

}
