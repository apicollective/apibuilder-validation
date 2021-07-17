package io.apibuilder.validation

import io.apibuilder.spec.v0.models.Method
import io.apibuilder.validation.helpers.Helpers
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec

class PathNormalizerSpec extends AnyFunSpec with Matchers with Helpers {

  private[this] lazy val apibuilderApiService = loadService("apibuilder-api-service.json")

  private[this] val normalizer = PathNormalizer(apibuilderApiService)

  it("parse static routes") {
    normalizer.resolve(Method.Get, "/non/existent/route/should/not/match") should be(
      Left(Seq("HTTP Operation 'GET /non/existent/route/should/not/match' is not defined"))
    )

    val getUsers = normalizer.resolve(Method.Get, "/users").toOption.get
    getUsers.method should equal(Method.Get)
    getUsers.path should equal("/users")

    val postOrganizations = normalizer.resolve(Method.Post, "/organizations").toOption.get
    postOrganizations.method should equal(Method.Post)
    postOrganizations.path should equal("/organizations")
  }

  it("parse dynamic routes") {
    normalizer.resolve(Method.Get, "/non/:foo/route/should/not/match") should be(
      Left(Seq("HTTP Operation 'GET /non/:foo/route/should/not/match' is not defined"))
    )

    val getUsersByGuid = normalizer.resolve(Method.Get, "/users/123").toOption.get
    getUsersByGuid.method should equal(Method.Get)
    getUsersByGuid.path should equal("/users/:guid")

    val putOrganizationsByGuid = normalizer.resolve(Method.Put, "/apicollective/apibuilder-api").toOption.get
    putOrganizationsByGuid.method should equal(Method.Put)
    putOrganizationsByGuid.path should equal("/:orgKey/:applicationKey")
  }

}
