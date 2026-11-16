[![Build Status](https://travis-ci.org/apicollective/apibuilder-validation.svg?branch=master)](https://travis-ci.org/apicollective/apibuilder-validation)

# apibuilder-validation

Accepts an instance of an [apibuilder
service](https://app.apibuilder.io/apicollective/apibuilder-spec/latest),
providing high quality validation of JSON readers with an emphasis on
providing human understandable error messages. Also handles safe
conversions of types (e.g. number to string) based on the API Builder
schema definition.

# Dependencies

play-json and play-json-joda are `Provided` dependencies, so add both to your own build.
play-json 2.x (`com.typesafe.play`) and 3.x (`org.playframework`) contain the same classes
and cannot share a classpath; the application decides which one it runs on.

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
