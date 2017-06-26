package io.apibuilder.validation

import org.scalatest.{Matchers, FunSpec}

class PathParserSpec extends FunSpec with Matchers {

  it("parse static routes") {
    PathParser.parse("") should be(
      ParsedPath("", "", Nil)
    )

    PathParser.parse("/") should be(
      ParsedPath("/", "/", Nil)
    )

    PathParser.parse("/foo") should be(
      ParsedPath("/foo", "/foo", Nil)
    )

    PathParser.parse("/foo/bar") should be(
      ParsedPath("/foo/bar", "/foo/bar", Nil)
    )

    PathParser.parse("/foo/bar.html") should be(
      ParsedPath("/foo/bar.html", "/foo/bar.html", Nil)
    )
  }

  it("parse with single var") {
    PathParser.parse("/users/:id") should be(
      ParsedPath("/users/:var", "/users/:id", Seq("id"))
    )

    PathParser.parse("/users/:id/show") should be(
      ParsedPath("/users/:var/show", "/users/:id/show", Seq("id"))
    )

    PathParser.parse("/users/:guid") should be(
      ParsedPath("/users/:var", "/users/:guid", Seq("guid"))
    )

    PathParser.parse("/users/:guid.html") should be(
      ParsedPath("/users/:var.html", "/users/:guid.html", Seq("guid"))
    )
  }

  it("parses up to first dot, ignoring later dots") {
    PathParser.parse("/users/:guid.tmp.html") should be(
      ParsedPath("/users/:var.tmp.html", "/users/:guid.tmp.html", Seq("guid"))
    )
  }

  it("parse with multiple var") {
    PathParser.parse("/organizations/:org/users/:id") should be(
      ParsedPath("/organizations/:var/users/:var", "/organizations/:org/users/:id", Seq("org", "id"))
    )
    PathParser.parse("/organizations/:org/users/:id.html") should be(
      ParsedPath("/organizations/:var/users/:var.html", "/organizations/:org/users/:id.html", Seq("org", "id"))
    )
  }

}
