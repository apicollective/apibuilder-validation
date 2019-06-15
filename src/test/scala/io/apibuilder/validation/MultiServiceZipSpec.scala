package io.apibuilder.validation

import java.io.File

import io.apibuilder.spec.v0.models.json._
import io.apibuilder.spec.v0.models.Service
import io.apibuilder.validation.zip.ZipFileBuilder
import play.api.libs.json._
import org.scalatest.{FunSpec, Matchers}

class MultiServiceZipSpec extends FunSpec with Matchers
  with helpers.ApiBuilderServiceHelpers
  with helpers.FileHelpers
  with helpers.PerformanceHelpers
  with helpers.Helpers
{

  private[this] lazy val zipService = MultiService.fromUrl("https://cdn.flow.io/util/lib-apibuilder/specs.zip").right.get

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
    zipService.validateOperation("GET", "/users").right.get
    flowMultiService.validateOperation("GET", "/users").right.get

    def run(testCase: String, service: MultiService) = {
      val result = time(100) { i =>
        service.validateOperation("GET", s"/users/$i")
      }
      println(s"$testCase: $result ms")
      result
    }
    run("api", flowMultiService)
    run("zip", zipService)
  }

  it("performance: resolving large number of operations") {
    zipService.validateOperation("GET", "/users/1").right.get
    flowMultiService.validateOperation("GET", "/users/1").right.get

    def run(testCase: String, service: MultiService) = {
      val ops = service.services().flatMap(_.service.resources.flatMap(_.operations))
      val result = time(100) { _ =>
        ops.foreach { op =>
          service.validateOperation(op.method, op.path)
        }
      }
      println(s"$testCase: $result ms")
      result
    }
    run("api", flowMultiService)
    run("zip", zipService)
  }

  it("upcast") {
    val op = zipService.operation("POST", "/users").get.operation
    op.body.map(_.`type`) should equal(Some("user_form"))
    zipService.upcastOperationBody("POST", "/users", Json.obj("name" -> "test")) should equal(
      Left(Seq("user_form.name must be an object and not a string"))
    )
  }
}
