package io.apibuilder.validation

import io.apibuilder.builders.ApiBuilderServiceBuilders
import io.apibuilder.helpers.{Helpers, TestHelpers}
import io.apibuilder.spec.v0.models.{Method, Operation}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PathNormalizerSpec extends AnyWordSpec with Matchers with Helpers with TestHelpers with ApiBuilderServiceBuilders{

  private val normalizer = PathNormalizer(ApiBuilderService(
    makeService(
      models = Seq(
        makeModel("user"),
        makeModel("organization"),
        makeModel("application"),
      ),
      resources = Seq(
        makeResource("user", operations = Seq(
          makeOperation(Method.Get, "/users"),
          makeOperation(Method.Get, "/users/:guid"),
        )),
        makeResource("organization", operations = Seq(
          makeOperation(Method.Post, "/organizations")
        )),
        makeResource("application", operations = Seq(
          makeOperation(Method.Put, "/:orgKey/:applicationKey"),
        ))
      )
    )
  ))
  private def resolveValid(method: Method, path: String): Operation = {
    expectValidNec {
      normalizer.resolve(method, path)
    }
  }
  private def resolveInvalid(method: Method, path: String): Seq[String] = {
    expectInvalidNec {
      normalizer.resolve(method, path)
    }
  }

  "parse static routes" in {
    resolveInvalid(Method.Get, "/non/existent/route/must/not/match") mustBe Seq("HTTP Operation 'GET /non/existent/route/must/not/match' is not defined")

    val getUsers = resolveValid(Method.Get, "/users")
    getUsers.method must equal(Method.Get)
    getUsers.path must equal("/users")

    val postOrganizations = resolveValid(Method.Post, "/organizations")
    postOrganizations.method must equal(Method.Post)
    postOrganizations.path must equal("/organizations")
  }

  "parse dynamic routes" in {
    resolveInvalid(Method.Get, "/non/:foo/route/must/not/match") mustBe Seq("HTTP Operation 'GET /non/:foo/route/must/not/match' is not defined")

    val getUsersByGuid = resolveValid(Method.Get, "/users/123")
    getUsersByGuid.method must equal(Method.Get)
    getUsersByGuid.path must equal("/users/:guid")

    val putOrganizationsByGuid = resolveValid(Method.Put, "/apicollective/apibuilder-api")
    putOrganizationsByGuid.method must equal(Method.Put)
    putOrganizationsByGuid.path must equal("/:orgKey/:applicationKey")
  }

}
