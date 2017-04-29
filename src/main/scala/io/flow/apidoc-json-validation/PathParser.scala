package io.flow.lib.apidoc.json.validation

case class ParsedPath(
  canonical: String,
  original: String,
  variables: Seq[String]
)

/**
  * Parsers a path, turning all variables into canonical names.
  */
object PathParser {

  private[this] val EmptyPath = ParsedPath(
    canonical = "",
    original = "",
    variables = Nil
  )

  private[this] val RootPath = ParsedPath(
    canonical = "/",
    original = "/",
    variables = Nil
  )

  def parse(path: String): ParsedPath = {
    val canonicalParts = scala.collection.mutable.ListBuffer[String]()
    val variables = scala.collection.mutable.ListBuffer[String]()

    path match {
      case "" => EmptyPath
      case "/" => RootPath
      case _ => path.split("/").foreach { p =>
        // Preserve file suffix - e.g. /:id.html => ("id", ".html")
        if (p.startsWith(":")) {
          val index = p.indexOf(".")
          val (prefix, suffix) = if (index < 1) {
            (p.substring(1), "")
          } else {
            (p.substring(1, index), p.substring(index))
          }

          variables.append(prefix)
          canonicalParts.append(s":var$suffix")
        } else {
          canonicalParts.append(p)
        }
      }

      ParsedPath(
        canonical = canonicalParts.mkString("/"),
        original = path,
        variables = variables
      )
    }
  }

}
