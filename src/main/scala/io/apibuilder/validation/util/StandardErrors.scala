package io.apibuilder.validation.util

import io.apibuilder.spec.v0.models.Method

object StandardErrors {

  def invalidMethodError(method: String): String = {
    s"HTTP method '$method' is invalid. Must be one of: " + Method.all.map(_.toString).mkString(", ")
  }
  def withTimer[T](label: String)(f: => T): T = {
    val start = System.currentTimeMillis()
    try {
      f
    } finally {
      val duration = System.currentTimeMillis() - start
      println(s"[Timer] $label $duration ms")
    }
  }

}