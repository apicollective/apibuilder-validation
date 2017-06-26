[![Build Status](https://travis-ci.org/flowcommerce/apibuilder-validation.svg?branch=master)](https://travis-ci.org/flowcommerce/apibuilder-validation)

# apibuilder-validation

Accepts an instance of an [apibuilder
aservice](https://app.apibuilder.io/apicollective/apibuilder-spec/latest),
providing high quality validation of JSON readers with an emphasis on
providing human understandable error messages. Also handles safe
conversions of types (e.g. number to string) based on the API Builder
schema definition.

# Usage

```
val multi = MultiService.fromUrls(
  Seq(
    "https://app.apibuilder.io/flow/api/latest/service.json",
    "https://app.apibuilder.io/apicollective/apibuilder-api/latest/service.json"
  )
) match {
  case Left(errors) => sys.error(s"Failed to load: $errors")
  case Right(m) => m
}

// If there is an error, return a very nice, human friendly error message.
// Otherwise 'upcast' the json object to one that matches the expectations
// set by the specification.
multi.upcast(
  "POST",
  "/:organization/webhooks",
  Json.obj("url" -> "https://test.flow.io")
) should equal(
  Left(Seq("Missing required field for type 'webhook_form': 'events'"))
)

// This js value will handle standard js type conversions (e.g. boolean -> string)
val js = multi.upcast(
  "POST",
  "/:organization/webhooks",
  Json.obj("url" -> "https://test.flow.io", "events" -> ["catalog_upserted"])
) match {
  case Left(errors) => println("Validation errors: " + errors)
  case Right(value) => value
)
```