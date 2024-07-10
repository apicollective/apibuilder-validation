package io.apibuilder.helpers

import cats.data.Validated.{Invalid, Valid}
import cats.data.{Validated, ValidatedNec}
import java.util.UUID

trait TestHelpers {

  def randomString(): String = UUID.randomUUID().toString

  def expectInvalid[T](value: Validated[T, ?]): T = {
    value match {
      case Invalid(e) => e
      case Valid(_) => sys.error("Expected invalid")
    }
  }

  def expectValid[T](value: Validated[?, T]): T = {
    value match {
      case Invalid(e) => sys.error(s"Expected right but got: ${e}")
      case Valid(v) => v
    }
  }

  def expectInvalidNec[T](value: ValidatedNec[T, ?]): Seq[T] = {
    value match {
      case Invalid(e) => e.toNonEmptyList.toList
      case Valid(_) => sys.error("Expected invalid")
    }
  }

  def expectValidNec[S, T](value: ValidatedNec[S, T]): T = {
    value match {
      case Invalid(e) => sys.error(s"Expected valid but got: ${e.toNonEmptyList.toList.mkString(", ")}")
      case Valid(v) => v
    }
  }

}
