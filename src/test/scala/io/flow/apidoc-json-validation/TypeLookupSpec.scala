package io.flow.lib.apidoc.json.validation

import com.bryzek.apidoc.spec.v0.models.{Contact, Service}
import com.bryzek.apidoc.spec.v0.models.json._
import play.api.libs.json._
import org.scalatest.{FunSpec, Matchers}

class TypeLookupSpec extends FunSpec with Matchers {

  lazy val service = {
    val contents = scala.io.Source.fromFile("src/test/resources/flow-api-service.json", "UTF-8").getLines.mkString("\n")
    Json.parse(contents).as[Service]
  }

  lazy val typeLookup = TypeLookup(service)

  it("unknown path") {
    typeLookup.forPath("FOO", "/other") should be(None)
  }

  it("resolves for known paths") {
    typeLookup.forPath("POST", "/users") should be(Some("user_form"))
    typeLookup.forPath("post", "/:organization/orders") should be(Some("order_form"))
    typeLookup.forPath("put", "/:organization/orders/:number") should be(Some("order_put_form"))
  }
}
