# Added example definitiona

```
apibuilder upload apicollective apibuilder-explicit-validation-core ./apibuilder-explicit-validation-core.json --version `sem-info tag latest`
curl -o ./src/test/resources/apibuilder-explicit-validation-core-service.json https://app.apibuilder.io/apicollective/apibuilder-explicit-validation-core/latest/service.json

apibuilder upload apicollective apibuilder-explicit-validation ./apibuilder-explicit-validation.json --version `sem-info tag latest`
curl -o ./src/test/resources/apibuilder-explicit-validation-service.json https://app.apibuilder.io/apicollective/apibuilder-explicit-validation/latest/service.json
```


