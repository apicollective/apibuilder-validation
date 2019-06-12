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
  with helpers.Helpers
{

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
    multiService.findType(service1.service.models.head.name).headOption.getOrElse {
      sys.error("Failed to find service 1 model")
    }
    multiService.findType(service2.service.models.head.name).headOption.getOrElse {
      sys.error("Failed to find service 2 model")
    }
  }

  it("Able to download service from the internet") {
    MultiService.fromUrl("https://cdn.flow.io/util/lib-apibuilder/specs.zip") match {
      case Left(errors) => sys.error(s"Errors: $errors")
      case Right(_) => // no-op
    }
  }

  it("performance is similar") {
    val zipService = MultiService.fromUrl("https://cdn.flow.io/util/lib-apibuilder/specs.zip").right.get
    zipService.validate("GET", "/users").right.get
    flowMultiService.validate("GET", "/users").right.get

    val a = time(500) {
      flowMultiService.validate("GET", "/users")
    }
    val b = time(500) {
      zipService.validate("GET", "/users")
    }
    println(s"A: $a ms")
    println(s"B: $b ms")
    Math.abs(a-b)/a < .1 should be(true)
  }

  private[this] def time(numberIterations: Int)(f: => Any): Long = {
    val start = System.currentTimeMillis()
    0.to(numberIterations).foreach { _ =>
      f
    }
    System.currentTimeMillis() - start
  }
}
