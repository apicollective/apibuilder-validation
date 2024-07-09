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
      r.code match {
        case ResponseCodeInt(s) => s == responseCode
        case _ => false
      }
    } match {
      case Some(r) => {
        Some(r)
      }

      case None => {
        operation.responses.find { r =>
          r.code match {
            case ResponseCodeOption.Default => true
            case _ => false
          }
        }
      }
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
    val responseCodes = operation.responses.map(_.code)
    if (responseCodes.exists {
      case ResponseCodeOption.Default => true
      case _ => false
    }) {
      // All response codes are valid
      Seq("*")

    } else {
      responseCodes.flatMap {
        case ResponseCodeOption.Default => None
        case ResponseCodeOption.UNDEFINED(_) => None
        case ResponseCodeInt(value) => Some(value.toString)
        case ResponseCodeUndefinedType(_) => None
      }
    }
  }
}

