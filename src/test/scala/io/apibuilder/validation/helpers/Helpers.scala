package io.apibuilder.validation.helpers

import java.io.File
import io.apibuilder.spec.v0.models.Service
import io.apibuilder.spec.v0.models.json._
import io.apibuilder.validation.zip.FileUtil
import io.apibuilder.validation.{AnyType, ApiBuilderService, ApiBuilderType, MultiService, MultiServiceImpl}
import play.api.libs.json.Json

trait Helpers {

  def readFile(filename: String): String = {
    FileUtil.readFileAsString(new File("src/test/resources/" + filename))
  }

  def loadService(filename: String): ApiBuilderService = {
    ApiBuilderService(
      Json.parse(readFile(filename)).as[Service]
    )
  }

  def loadMultiService(files: List[String]): MultiService = {
    MultiServiceImpl(files.map(loadService))
  }

  def mustFindType(multiService: MultiService, qualified: String): AnyType = {
    multiService.findType(qualified).getOrElse {
      sys.error(s"Failed to find type '$qualified")
    }
  }

  def mustFindModelType(multiService: MultiService, qualified: String): ApiBuilderType.Model = {
    mustFindType(multiService, qualified).asInstanceOf[ApiBuilderType.Model]
  }

  lazy val apiBuilderMultiService: MultiService = {
    loadMultiService(
      List(
        "apibuilder-explicit-validation-core-service.json",
        "apibuilder-explicit-validation-service.json"
      )
    )
  }

  lazy val flowMultiService: MultiService = {
    loadMultiService(
      List(
        "flow-api-service.json",
        "flow-api-internal-service.json"
      )
    )
  }

  def rightOrErrors[_,V](f: Either[_, V]): V = {
    f match {
      case Left(bad) => sys.error(s"Expected valid value but got: $bad")
      case Right(v) => v
    }
  }

}
