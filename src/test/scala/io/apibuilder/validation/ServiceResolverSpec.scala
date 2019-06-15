package io.apibuilder.validation

import java.io.File

import io.apibuilder.spec.v0.models.Service
import io.apibuilder.spec.v0.models.json._
import io.apibuilder.validation.zip.ZipFileBuilder
import org.scalatest.{FunSpec, Matchers}
import play.api.libs.json._

class ServiceResolverSpec extends FunSpec with Matchers
  with helpers.PerformanceHelpers
{
  private[this] lazy val zipService = MultiService.fromUrl("https://cdn.flow.io/util/lib-apibuilder/specs.zip").right.get

  it("performance: doResolveService") {
    def run(testCase: String, service: MultiService) = {
      val result = time(1000) {
        service.services().flatMap(_.service.resources.flatMap(_.operations)).foreach { op =>
          service.validateOperation(op.method, op.path)
        }
      }
      println(s"$testCase: $result ms")
      result
    }

    run("zip", zipService)
  }
}
