package io.apibuilder.rewriter

import io.apibuilder.builders.{ApiBuilderServiceBuilders, MultiServiceBuilders}
import io.apibuilder.spec.v0.models.{Operation, Resource, Response, ResponseCodeInt}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FilterResponsesRewriterSpec extends AnyWordSpec with Matchers
  with ApiBuilderServiceBuilders
  with MultiServiceBuilders {

  private def resource(
    resourceType: String = random(),
    responseCodes: Seq[Int],
  ): Resource = {
    makeResource(
      `type` = resourceType,
      operations = Seq(
        makeOperation(
          responses = responseCodes.map { s =>
            makeResponse(code = ResponseCodeInt(s))
          },
        )
      )
    )
  }

  private def rewrite(resources: Seq[Resource])(filterResponses: Operation => Seq[Response]) = {
    FilterResponsesRewriter(filterResponses).rewrite(
      makeMultiService(
        makeService(resources = resources)
      )
    ).services().map(_.service).flatMap(_.resources)
  }

  private def responseCodes(operation: Operation): Seq[Int] = {
    operation.responses.map(_.code).flatMap {
      case c: ResponseCodeInt => Some(c.value)
      case _ => None
    }
  }

  private def responseCodes(resource: Resource): Seq[Int] = {
    resource.operations.flatMap(responseCodes)
  }

  private def responseCodes(resources: Seq[Resource]): Seq[Int] = {
    resources.flatMap(responseCodes)
  }

  "operations" must {
    def opByResponseCode(code: Int): Operation = {
      makeOperation(
        responses = Seq(
          makeResponse(code = ResponseCodeInt(code))
        ),
      )
    }

    "be removed when all responses are filtered" in {
      rewrite(
        Seq(
          makeResource(
            operations = Seq(
              opByResponseCode(200),
              opByResponseCode(201),
            )
          )
        )
      )(_.responses.filter(_.code == ResponseCodeInt(200))).flatMap(_.operations) match {
        case one :: Nil => {
          // verifying that only the operation with response code 200 was accepted
          responseCodes(one) must equal(Seq(200))
        }
        case other => sys.error(s"Expected exactly 1 resource but found: ${other.size}")
      }
    }
  }

  "resources" must {
    "be removed when all operations are filtered" in {
      rewrite(
        Seq(
          resource("a", responseCodes = Seq(200, 201)),
          resource("b", responseCodes = Seq(200)),
        )
      )(_.responses.filter(_.code == ResponseCodeInt(201))) match {
        case one :: Nil => {
          one.`type` must equal("a")
          responseCodes(one) must equal(Seq(201))
        }
        case other => sys.error(s"Expected exactly 1 resource but found: ${other.size}")
      }
    }
  }

  "responses" must {

    "filter" in {
      def acceptCode(code: Int) = {
        responseCodes(
          rewrite(
            Seq(
              resource(responseCodes = Seq(200, 400))
            )
          )(_.responses.filter(_.code == ResponseCodeInt(code)))
        )
      }
      acceptCode(200) must be(Seq(200))
      acceptCode(400) must be(Seq(400))
      acceptCode(500) must be(Nil)
    }
  }

}
