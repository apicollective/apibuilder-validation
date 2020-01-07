package io.apibuilder.validation.util

import cats.implicits._
import cats.data.NonEmptyChain

object Implicits {

  implicit class EitherOps[E, V](val v: Either[NonEmptyChain[E], V]) extends AnyVal {
    def leftToSeq: Either[Seq[E], V] = v.left.map(_.toList)
  }

}
