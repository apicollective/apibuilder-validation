package io.apibuilder.validation.helpers

trait PerformanceHelpers {

  def time(numberIterations: Int)(f: Long => Any): Long = {
    val start = System.currentTimeMillis()
    0.to(numberIterations).foreach { i =>
      f(i)
    }
    System.currentTimeMillis() - start
  }

}

