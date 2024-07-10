package io.apibuilder.validation

import io.apibuilder.spec.v0.models._

trait ResponseHelpers {

  def response(apibuilderOperation: ApiBuilderOperation, responseCode: Int): Option[Response] = {
    response(apibuilderOperation.operation, responseCode)
  }

  /**
    * Looks up the response for the given status code for this operation, or None
    * if there is no response documented for the status code
    */
  def response(operation: Operation, responseCode: Int): Option[Response] = {
    operation.responses.find { r =>
      r.code.toIntOption.contains(responseCode)
    } orElse {
      operation.responses.find(_.code.toLowerCase == "default")
    }
  }

  def validateResponseCode(apiBuilderOperation: ApiBuilderOperation, responseCode: Int): Either[String, Response] = {
    validateResponseCode(apiBuilderOperation.operation, responseCode)
  }

  /**
    * If the responseCode is valid for the operation, returns a Right(Unit) - otherwise
    * returns an error message detailing the difference in expectation.
    */
  def validateResponseCode(operation: Operation, responseCode: Int): Either[String, Response] = {
    response(operation, responseCode) match {
      case Some(r) => {
        Right(r)
      }

      case None => {
        Left(
          s"Unexpected response code[$responseCode] for operation[${operation.method} ${operation.path}]. Declared response codes: " +
            declaredResponseCodes(operation).mkString(", ")
        )
      }
    }
  }

  /**
    * Returns a list of the declared response codes. If ALL response codes are valid,
    * will return ["*"]
    */
  private def declaredResponseCodes(operation: Operation): Seq[String] = {
    val responseCodes = operation.responses.map(_.code.toLowerCase)
    if (responseCodes.contains("default")) {
      // All response codes are valid
      Seq("*")

    } else {
      responseCodes
    }
  }
}

