package io.apibuilder.validation

import io.apibuilder.builders.ApiBuilderServiceBuilders
import io.apibuilder.helpers._
import io.apibuilder.spec.v0.models.json._
import io.apibuilder.spec.v0.models.{Method, Service}
import io.apibuilder.validation.zip.ZipFileBuilder
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json._

import java.io.File

class MultiServiceZipSpec extends AnyWordSpec with Matchers
  with ApiBuilderServiceBuilders
  with FileHelpers
  with Helpers
  with TestHelpers
{

  private case class ServiceAndFile(service: Service, file: File)

  private def createServiceAndWriteToFile(service: Service): ServiceAndFile = {
    ServiceAndFile(
      service = service,
      file = writeToTempFile(Json.toJson(service).toString)
    )
  }

  private lazy val service1 = createServiceAndWriteToFile(makeService(
    models = Seq(
      makeModel("user"),
      makeModel("user_form", fields = Seq(makeField("name", `type` = "object"))),
    ),
    resources = Seq(
      makeResource("user", operations = Seq(
        makeOperation(Method.Post, "/users", body = Some(makeBody("user_form")))
      ))
    )
  ))
  private lazy val service2 = createServiceAndWriteToFile(makeService(
    models = Seq(makeModel("guest"))
  ))
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
    multiService.findType(service1.service.namespace, "user").getOrElse {
      sys.error("Failed to find service 1 model")
    }
    multiService.findType(service2.service.namespace, "guest").getOrElse {
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
    op.body.get.`type` must equal("user_form")
    expectInvalidNec {
      multiService.upcastOperationBody("POST", "/users", Json.obj("name" -> "test"))
    } mustBe Seq("user_form.name must be an object and not a string")
  }
}
