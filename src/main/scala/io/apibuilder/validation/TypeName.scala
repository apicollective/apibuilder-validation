package io.apibuilder.validation

case class TypeName(
  name: String,
  namespace: String
)

/**
  * Parses a type (like 'user' or 'io.flow.common.v0.models.user') into a
  * namespace and name
  */
object TypeName {

  def parse(name: String, defaultNamespace: String): TypeName = {
    val parts = name.split("\\.").toList

    if (parts.lengthCompare(2) >= 0) {
      TypeName(
        name = parts.last,
        namespace = parts.slice(0, parts.length-2).mkString(".")
      )
    } else {
      TypeName(
        name = parts.last,
        namespace = defaultNamespace
      )
    }
  }

}
