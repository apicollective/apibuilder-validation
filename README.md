[![Build Status](https://travis-ci.org/flowcommerce/lib-apidoc-json-validation.svg?branch=master)](https://travis-ci.org/flowcommerce/lib-apidoc-json-validation)

# lib-apidoc-json-validation

Accepts an instance of an [apidoc
service](http://apidoc.me/bryzek/apidoc-spec/latest), providing high
quality validation of JSON readers with an emphasis on providing human
understandable error messages.

# Usage

```
val multi = MultiService.fromUrls(
  Seq(
    "http://apidoc.me/flow/api/latest/service.json",
    "http://apidoc.me/bryzek/apidoc-api/latest/service.json"
  )
) match {
  case Left(errors) => sys.error(s"Failed to load: $errors")
  case Right(m) => m
}

// If there is an error, return a very nice, human friendly error message
multi.validate(
  "POST",
  "/:organization/webhooks",
  Json.obj("url" -> "https://test.flow.io")
) should equal(
  Left(Seq("Missing required field for type 'webhook_form': 'events'"))
)

// This js value will handle standard js type conversions (e.g. boolean -> string)
val js = multi.validate(
  "POST",
  "/:organization/webhooks",
  Json.obj("url" -> "https://test.flow.io", "events" -> ["catalog_upserted"])
) match {
  case Left(errors) => println("Validation errors: " + errors)
  case Right(value) => value
)
```