package io.apibuilder.validation

import java.io.File

import org.scalatest.{FunSpec, Matchers}
import play.api.libs.json._

class FooSpec extends FunSpec with Matchers {

  it("handles empty strings") {
    val item = Json.parse(
      """
        |{
        |  "number": "41546",
        |  "name": "Lark Long Sleeve",
        |  "description": "The Lark Long Sleeve by Alo Yoga in gravel heather is a fitted long sleeve performance top with ventilated mesh details, seamless knit construction and slash details for a little edge.",
        |  "locale": "en_US",
        |  "price": 74.00,
        |  "currency": "USD",
        |  "categories": ["Apparel", "Sweaters"],
        |  "attributes": {
        |    "color": "Gravel Heather",
        |    "size": "S",
        |    "fit_and_feel": "Fabric Details: 64% Nylon 27% Polyester 9% Spandex\n-Seamless knit construction for extra flexibility\n-Mesh gives extra breathability for\n your practice\n-Dry wicking, anti microbial, 4-way stretch\n-Fits true to size\n-Model is wearing size small",
        |    "countries_of_origin": "CHN",
        |    "product_id": "lark-long-sleeve-gravel-heather",
        |    "gender": "female",
        |    "msrp": "74.00",
        |    "brand": "Alo Yoga"
        |  },
        |  "images": [
        |    { "url": "https://d10d8p3bp9wpno.cloudfront.net/media/catalog/product/cache/1/thumbnail/120x160/9df78eab33525d08d6e5fb8d27136e95/1/-/1-alo-lark-long-sleeve-top-gravel-heather.jpg", "tags": ["thumbnail"] }
        |  ],
        |  "dimensions": {
        |    "package": {
        |      "weight": { "value": "1.5", "units": "pound" },
        |      "length": { "value": "18", "units": "inch" },
        |      "width": { "value": "18", "units": "inch" },
        |      "height": { "value": "1", "units": "foot" }
        |    },
        |    "product": {
        |      "weight": { "value": "1", "units": "pound" },
        |      "diameter": { "value": "12", "units": "inch" }
        |    }
        |  }
        |}
      """.stripMargin
    )
    println("YES")
    println(FormData.toEncoded(item))
    val parts = FormData.toEncoded(item).split("&").map(_.split("="))
    println(parts.map(_.head).mkString(","))
    println(parts.map(_.last).mkString(","))
  }

}
