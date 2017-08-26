package io.apibuilder.validation

import io.apibuilder.spec.v0.models.Method
import org.scalatest.{FunSpec, Matchers}

class PathNormalizerSpec extends FunSpec with Matchers {

  private[this] lazy val apibuilderApiService = {
    val base = "file://" + new java.io.File(".").getAbsolutePath
    ApiBuilderService.fromUrl(s"$base/src/test/resources/apibuilder-api-service.json") match {
      case Left(errors) => sys.error(s"Failed to load: $errors")
      case Right(s) => s
    }
  }

  private[this] val normalizer = PathNormalizer(apibuilderApiService)

  it("parse static routes") {
    normalizer.resolve(Method.Get, "/non/existent/route/should/not/match") should be(None)

    val getUsers = normalizer.resolve(Method.Get, "/users").get
    getUsers.method should equal(Method.Get)
    getUsers.path should equal("/users")

    val postOrganizations = normalizer.resolve(Method.Post, "/organizations").get
    postOrganizations.method should equal(Method.Post)
    postOrganizations.path should equal("/organizations")
  }

  it("parse dynamic routes") {
    normalizer.resolve(Method.Get, "/non/:foo/route/should/not/match") should be(None)

    val getUsersByGuid = normalizer.resolve(Method.Get, "/users/123").get
    getUsersByGuid.method should equal(Method.Get)
    getUsersByGuid.path should equal("/users/:guid")

    val putOrganizationsByGuid = normalizer.resolve(Method.Put, "/apicollective/apibuilder-api").get
    putOrganizationsByGuid.method should equal(Method.Put)
    putOrganizationsByGuid.path should equal("/:orgKey/:applicationKey")
  }

}
