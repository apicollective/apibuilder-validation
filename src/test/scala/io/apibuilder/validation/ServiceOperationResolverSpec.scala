package io.apibuilder.validation

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.apibuilder.builders.ApiBuilderServiceBuilders
import io.apibuilder.spec.v0.models.json._
import io.apibuilder.helpers.*
import io.apibuilder.validation.zip.ZipFileBuilder
import play.api.libs.json.Json

import java.io.File

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

    val services = 0.to(100).map { _ =>
      makeService()
    }
    val zipFile = services.foldLeft(ZipFileBuilder()) { case (builder, svc) =>
      builder.withFile(writeToTempFile(Json.toJson(svc).toString, suffix = "json"))
    }.build()

    val zipService = expectValidNec {
      MultiService.fromUrl(s"file://${zipFile.getAbsolutePath}")
    }
    run("zipService", zipService)
  }
}
