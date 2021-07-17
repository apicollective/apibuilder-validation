package io.apibuilder.validation

import java.io.File

import io.apibuilder.builders.ApiBuilderServiceBuilders
import io.apibuilder.spec.v0.models.json._
import io.apibuilder.spec.v0.models.{Method, Service}
import io.apibuilder.validation.zip.ZipFileBuilder
import play.api.libs.json._
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec

class MultiServiceZipSpec extends AnyFunSpec with Matchers
  with ApiBuilderServiceBuilders
  with helpers.FileHelpers
  with helpers.PerformanceHelpers
  with helpers.Helpers
{

  private[this] lazy val zipService = MultiService.fromUrl("https://cdn.flow.io/util/lib-apibuilder/specs.zip").toOption.get

  case class ServiceAndFile(service: Service, file: File)

  def createServiceAndWriteToFile(): ServiceAndFile = {
    val service = makeService(
      models = Seq(makeModel())
    )
    ServiceAndFile(
      service = service,
      file = writeToTempFile(Json.toJson(service).toString)
    )
  }

  private[this] lazy val service1 = createServiceAndWriteToFile()
  private[this] lazy val service2 = createServiceAndWriteToFile()
  private[this] lazy val zipFile: File = {
    ZipFileBuilder()
      .withFile(service1.service.name + ".json", service1.file)
      .withFile(service2.service.name + ".json", service2.file)
      .build()
  }

  it("loads multi service from zip file") {
    val multiService = MultiService.fromUrl(s"file://$zipFile") match {
      case Left(errors) => sys.error(s"Failed to load $zipFile: $errors")
      case Right(ms) => ms
    }
    multiService.findType(service1.service.namespace, service1.service.models.head.name).getOrElse {
      sys.error("Failed to find service 1 model")
    }
    multiService.findType(service2.service.namespace, service2.service.models.head.name).getOrElse {
      sys.error("Failed to find service 2 model")
    }
  }

  it("Able to download service from the internet") {
    MultiService.fromUrl("https://cdn.flow.io/util/lib-apibuilder/specs.zip") match {
      case Left(errors) => sys.error(s"Errors: $errors")
      case Right(_) => // no-op
    }
  }

  it("performance: validateOperation") {
    zipService.findOperation("GET", "/users/1").get
    flowMultiService.findOperation("GET", "/users/1").get

    def run(testCase: String, service: MultiService) = {
      val result = time(5000) { i =>
        service.findOperation(Method.Get, s"/users/$i")
      }
      println(s"performance: validateOperation: $testCase: $result ms")
      result
    }
    run("api", flowMultiService)
    run("zip", zipService)
  }

  it("upcast") {
    val op = zipService.findOperation("POST", "/users").get.operation
    op.body.map(_.`type`) should equal(Some("user_form"))
    zipService.upcastOperationBody("POST", "/users", Json.obj("name" -> "test")) should equal(
      Left(Seq("user_form.name must be an object and not a string"))
    )
  }
}
