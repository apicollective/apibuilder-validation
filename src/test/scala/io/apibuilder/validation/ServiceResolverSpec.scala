package io.apibuilder.validation

import org.scalatest.{FunSpec, Matchers}

class ServiceResolverSpec extends FunSpec with Matchers
  with helpers.PerformanceHelpers
{
  private[this] lazy val zipService = MultiService.fromUrl("https://cdn.flow.io/util/lib-apibuilder/specs.zip").right.get

  it("performance: doResolveService") {
    def run(testCase: String, multi: MultiService) = {
      val resolver = ServiceResolver(multi.services())
      val operations = multi.services().flatMap(_.service.resources.flatMap(_.operations))
      val result = time(100) {
        operations.foreach { op =>
          resolver.doResolveService(op.method, op.path)
        }
      }
      println(s"$testCase [${operations.length} operations]: $result ms")
      result
    }

    run("zip", zipService)
  }
}
