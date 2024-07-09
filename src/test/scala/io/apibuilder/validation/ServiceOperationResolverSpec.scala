package io.apibuilder.validation

import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec

class ServiceOperationResolverSpec extends AnyFunSpec with Matchers
  with helpers.PerformanceHelpers
  with helpers.Helpers
{
  private lazy val zipService = MultiService.fromUrl("https://cdn.flow.io/util/lib-apibuilder/specs.zip").toOption.get

  it("performance: doResolveService") {
    def run(testCase: String, multi: MultiService) = {
      val resolver = ServiceOperationResolver(multi.services())
      val operations = multi.services().flatMap(_.service.resources.flatMap(_.operations))
      val result = time(10) { i =>
        operations.foreach { op =>
          resolver.findOperation(op.method, op.path.replaceAll(":organization", i.toString))
        }
      }
      println(s"$testCase [${operations.length} operations]: $result ms")
      result
    }

    run("flowMultiService", flowMultiService)
    run("zipService", zipService)
  }
}
