package io.apibuilder.rewriter

import io.apibuilder.builders.{ApiBuilderServiceBuilders, MultiServiceBuilders}
import io.apibuilder.spec.v0.models.{Operation, Resource}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FilterOperationsRewriterSpec extends AnyWordSpec with Matchers
  with ApiBuilderServiceBuilders
  with MultiServiceBuilders
{
  private def op(attributeName: Option[String]): Operation = {
    makeOperation(
      attributes = attributeName.toSeq.map { n => makeAttribute(n) },
    )
  }

  // Filters resources based on presence of an attribute
  private def rewrite(resources: Seq[Resource]) = {
    FilterOperationsRewriter { op =>
      Some(op).filter(_.attributes.nonEmpty)
    }.rewrite(
      makeMultiService(
        makeService(resources = resources)
      )
    ).services().map(_.service).flatMap(_.resources)
  }

  "operations" must {
    "remove resources when all their operations are filtered" in {
      rewrite(
        Seq(
          makeResource(operations = Seq(op(None)))
        )
      ) must be(Nil)
    }

    "keeps resources when all their operations are accepted" in {
      rewrite(
        Seq(
          makeResource(operations = Seq(op(Some(random()))))
        )
      ) match {
        case r :: Nil => {
          r.operations.size must be(1)
        }
        case other => sys.error(s"Expected 1 resource but found ${other.size}")
      }
    }

    "keeps resources when at least one operation has an attribute" in {
      rewrite(
        Seq(
          makeResource(operations = Seq(
            op(None),
            op(Some(random())))
          )
        )
      ) match {
        case r :: Nil => {
          r.operations.size must be(1)
          r.operations.head.attributes.size must be(1)
        }
        case other => sys.error(s"Expected 1 resource but found ${other.size}")
      }
    }

  }
}
