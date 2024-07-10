package io.apibuilder.validation

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.apibuilder.builders.ApiBuilderServiceBuilders
import io.apibuilder.spec.v0.models.json.*
import io.apibuilder.helpers.*
import io.apibuilder.spec.v0.models.Method
import io.apibuilder.validation.zip.ZipFileBuilder
import play.api.libs.json.Json

class ServiceOperationResolverSpec extends AnyWordSpec with Matchers
  with PerformanceHelpers
  with Helpers
  with FileHelpers
  with TestHelpers
  with ApiBuilderServiceBuilders
{

  "performance: doResolveService" in {
    def run(testCase: String, multi: MultiService) = {
      val resolver = ServiceOperationResolver(multi.services)
      val operations = multi.services.flatMap(_.service.resources.flatMap(_.operations))
      val result = time(10) { i =>
        operations.foreach { op =>
          resolver.findOperation(op.method, op.path.replaceAll(":organization", i.toString))
        }
      }
      println(s"$testCase [${operations.length} operations]: $result ms")
      result
    }

    val models = 0.to(100).map { i => s"model_$i" }
    val services = 0.to(99).map { _ =>
      makeService(
        models = models.map { name => makeModel(name) },
        resources = models.map { name =>
          makeResource(name, operations = Method.all.map { m =>
            makeOperation(m)
          })
        }
      )
    }

    val zipFile = services.foldLeft(ZipFileBuilder()) { case (builder, svc) =>
      builder.withFile(writeToTempFile(Json.toJson(svc).toString, suffix = "json"))
    }.build()

    val zipService = expectValidNec {
      MultiService.fromUrl(s"file://${zipFile.getAbsolutePath}")
    }
    zipService.services.length mustBe 100
    run("zipService", zipService)
  }
}
