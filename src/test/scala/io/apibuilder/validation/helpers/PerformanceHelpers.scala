package io.apibuilder.validation.helpers

trait PerformanceHelpers {

  def time(numberIterations: Int)(f: => Any): Long = {
    val start = System.currentTimeMillis()
    0.to(numberIterations).foreach { _ =>
      f
    }
    System.currentTimeMillis() - start
  }

}

