# Added example definitiona

```
apibuilder upload apicollective apibuilder-union-discriminator-value ./apibuilder-union-discriminator-value.json --version `sem-info tag latest`

apibuilder upload apicollective apibuilder-explicit-validation-core ./apibuilder-explicit-validation-core.json --version `sem-info tag latest`

apibuilder upload apicollective apibuilder-explicit-validation ./apibuilder-explicit-validation.json --version `sem-info tag latest`

apibuilder download apicollective apibuilder-union-discriminator-value latest service > ./src/test/resources/apibuilder-union-discriminator-value.json

apibuilder download apicollective apibuilder-explicit-validation-core latest service > ./src/test/resources/apibuilder-explicit-validation-core-service.json

apibuilder download apicollective apibuilder-explicit-validation latest service > ./src/test/resources/apibuilder-explicit-validation-service.json

apibuilder download flow api latest service > ./src/test/resources/flow-api-service.json

apibuilder download flow api-internal latest service > ./src/test/resources/flow-api-internal-service.json
```
