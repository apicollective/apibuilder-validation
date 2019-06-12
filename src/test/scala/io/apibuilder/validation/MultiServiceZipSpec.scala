package io.apibuilder.validation

import java.io.File

import io.apibuilder.spec.v0.models.json._
import io.apibuilder.spec.v0.models.Service
import io.apibuilder.validation.util.ZipFileBuilder
import play.api.libs.json._
import org.scalatest.{FunSpec, Matchers}

class MultiServiceZipSpec extends FunSpec with Matchers
  with helpers.ApiBuilderServiceHelpers
  with helpers.FileHelpers {

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

  private[this] lazy val zipFile: File = {
    val service1 = createServiceAndWriteToFile()
    val service2 = createServiceAndWriteToFile()
    ZipFileBuilder()
      .withFile(service1.service.name, service1.file)
      .withFile(service2.service.name, service2.file)
      .build()
  }

  it("loads multi service from zip file") {
    println(s"zipFile: $zipFile")
  }
}