package io.apibuilder.validation

import org.scalatest.{FunSpec, Matchers}

class ServiceOperationResolverSpec extends FunSpec with Matchers
  with helpers.PerformanceHelpers
  with helpers.Helpers
{
  private[this] lazy val zipService = MultiService.fromUrl("https://cdn.flow.io/util/lib-apibuilder/specs.zip").right.get

  it("performance: doResolveService") {
    def run(testCase: String, multi: MultiService) = {
      val resolver = ServiceOperationResolver(multi.services())
      val operations = multi.services().flatMap(_.service.resources.flatMap(_.operations))
      val result = time(2) { i =>
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
