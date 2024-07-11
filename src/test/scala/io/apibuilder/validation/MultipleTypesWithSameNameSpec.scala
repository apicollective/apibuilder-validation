package io.apibuilder.validation

import io.apibuilder.builders.ApiBuilderServiceBuilders
import io.apibuilder.helpers.{Helpers, TestHelpers}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.Json

class MultipleTypesWithSameNameSpec extends AnyWordSpec with Matchers with Helpers with TestHelpers with ApiBuilderServiceBuilders {

  "validates type found in multiple services" in {
    // service1: item.price has an amount
    // service2: product.price has an amount and a currency
    val service1 = makeService(
      models = Seq(
        makeModel(
          name = "price",
          fields = Seq(
            makeField("amount")
          )
        ),
        makeModel(
          name = "item",
          fields = Seq(
            makeField("value", `type` = "price")
          )
        )
      )
    )

    val service2 = makeService(
      models = Seq(
        makeModel(
          name = "price",
          fields = Seq(
            makeField("amount"),
            makeField("currency")
          )
        ),
        makeModel(
          name = "product",
          fields = Seq(
            makeField("value", `type` = "price")
          )
        )
      )
    )
    val multi = MultiServiceImpl(
      List(
        ApiBuilderService(service1), ApiBuilderService(service2)
      )
    )

    expectInvalidNec {
      multi.upcast(
        mustFindModel(multi, service1.namespace, "item"),
        Json.obj("value" -> Json.obj())
      )
    } mustBe Seq("Missing required field for price: amount")

    expectInvalidNec {
      multi.upcast(
        mustFindModel(multi, service2.namespace, "product"),
        Json.obj("value" -> Json.obj())
      )
    } mustBe Seq("Missing required fields for price: amount, currency")
  }

}
