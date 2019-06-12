package io.apibuilder.validation.util

import java.io.File

import io.apibuilder.validation.zip.FileUtil

/**
  * Reads a file named "order.txt" in the zip file and uses that
  * to sort files in the order in which they should be loaded
  * into the MultiService class
 */
case class FileOrder(file: Option[File]) {
  private[this] val nameToOrder: Map[String, Int] = file match {
    case None => Map.empty
    case Some(f) => {
      Map(
        FileUtil
          .readFileAsString(f)
          .split("\n")
          .map(_.trim)
          .filterNot(_.isEmpty)
          .zipWithIndex.map { case (name, i) =>
            name.trim.toLowerCase() -> i
        }: _*
      )
    }
  }

  def sort(names: Seq[String]): List[String] = {
    names.sortBy(sortOrder).toList
  }

  def sortOrder(fileName: String): (Int, String) = {
    val lower = fileName.trim.toLowerCase()
    (
      nameToOrder.getOrElse(lower, Integer.MAX_VALUE),
      lower
    )
  }
}