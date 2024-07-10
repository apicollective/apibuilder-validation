package io.apibuilder.validation

import java.io.File

import io.apibuilder.builders.ApiBuilderServiceBuilders
import io.apibuilder.spec.v0.models.json._
import io.apibuilder.spec.v0.models.{Method, Service}
import io.apibuilder.validation.zip.ZipFileBuilder
import io.apibuilder.helpers._
import play.api.libs.json._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MultiServiceZipSpec extends AnyWordSpec with Matchers
  with ApiBuilderServiceBuilders
  with FileHelpers
  with Helpers
  with TestHelpers
{

  private case class ServiceAndFile(service: Service, file: File)

  private def createServiceAndWriteToFile(): ServiceAndFile = {
    val service = makeService(
      models = Seq(makeModel())
    )
    ServiceAndFile(
      service = service,
      file = writeToTempFile(Json.toJson(service).toString)
    )
  }

  private lazy val service1 = createServiceAndWriteToFile()
  private lazy val service2 = createServiceAndWriteToFile()
  private lazy val zipFile: File = {
    ZipFileBuilder()
      .withFile(service1.service.name + ".json", service1.file)
      .withFile(service2.service.name + ".json", service2.file)
      .build()
  }

  "loads multi service from zip file" in {
    val multiService = expectValidNec {
      MultiService.fromUrl(s"file://$zipFile")
    }
    multiService.findType(service1.service.namespace, service1.service.models.head.name).getOrElse {
      sys.error("Failed to find service 1 model")
    }
    multiService.findType(service2.service.namespace, service2.service.models.head.name).getOrElse {
      sys.error("Failed to find service 2 model")
    }
  }

  "Able to download service from the internet" in {
    expectValidNec {
      MultiService.fromUrl("https://cdn.flow.io/util/lib-apibuilder/specs.zip")
    }
  }

  "upcast" in {
    val multiService = expectValidNec {
      MultiService.fromUrl(s"file://$zipFile")
    }

    val op = multiService.findOperation("POST", "/users").get.operation
    op.body.map(_.`type`) must equal(Some("user_form"))
    multiService.upcastOperationBody("POST", "/users", Json.obj("name" -> "test")) must equal(
      Left(Seq("user_form.name must be an object and not a string"))
    )
  }
}
