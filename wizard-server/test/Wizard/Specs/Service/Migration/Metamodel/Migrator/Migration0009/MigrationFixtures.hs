module Wizard.Specs.Service.Migration.Metamodel.Migrator.Migration0009.MigrationFixtures where

import Data.Aeson.QQ

addIntegrationEventIn1 =
  [aesonQQ|
    {
      "annotations": {
        "test": "value"
      },
      "requestUrl": "${apiUrl}/search/?q=${q}&registry=${registry}",
      "uuid": "82090e7d-7aa8-4339-a8c5-2330c27ae147",
      "responseIdField": "record_id",
      "entityUuid": "f3520558-707e-415a-8878-c7b8d6ccc2fb",
      "props": [
        "registry"
      ],
      "parentUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
      "eventType": "AddIntegrationEvent",
      "name": "FAIRsharing",
      "requestHeaders": {
        "Accept": "application/json",
        "Api-Key": "${apiKey}"
      },
      "id": "fairsharing",
      "responseItemUrl": "https://fairsharing.org/${id}",
      "responseListField": "results",
      "logo": "exampleLogo",
      "responseNameField": "name",
      "requestMethod": "GET",
      "requestBody": ""
    }
  |]

addIntegrationEventOut1 =
  [aesonQQ|
    {
      "annotations": {
        "test": "value"
      },
      "requestUrl": "${apiUrl}/search/?q=${q}&registry=${registry}",
      "uuid": "82090e7d-7aa8-4339-a8c5-2330c27ae147",
      "responseItemId": "{{item.record_id}}",
      "entityUuid": "f3520558-707e-415a-8878-c7b8d6ccc2fb",
      "props": [
        "registry"
      ],
      "parentUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
      "eventType": "AddIntegrationEvent",
      "name": "FAIRsharing",
      "requestHeaders": {
        "Accept": "application/json",
        "Api-Key": "${apiKey}"
      },
      "id": "fairsharing",
      "responseItemUrl": "https://fairsharing.org/${id}",
      "responseListField": "results",
      "logo": "exampleLogo",
      "responseItemTemplate": "{{item.name}}",
      "requestMethod": "GET",
      "requestBody": ""
    }
  |]

editIntegrationEventIn1 =
  [aesonQQ|
    {
      "annotations": {
        "changed": true,
        "value": {
          "test": "testValue"
        }
      },
      "requestUrl": {
        "changed": true,
        "value": "https://query.wikidata.org/sparql?example"
      },
      "uuid": "d59d4a54-eb95-401d-8b74-55e8b4742943",
      "responseIdField": {
        "changed": false
      },
      "entityUuid": "b2ae610d-8dd1-4c4d-bf78-5a3c9f00e5d8",
      "props": {
        "changed": false
      },
      "parentUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
      "eventType": "EditIntegrationEvent",
      "name": {
        "changed": true,
        "value": "Wikidata"
      },
      "requestHeaders": {
        "changed": true,
        "value": {
          "testHeader": "testHeaderValue"
        }
      },
      "id": {
        "changed": false
      },
      "responseItemUrl": {
        "changed": false
      },
      "responseListField": {
        "changed": false
      },
      "logo": {
        "changed": false
      },
      "responseNameField": {
        "changed": true,
        "value": "label.value"
      },
      "requestMethod": {
        "changed": false
      },
      "requestBody": {
        "changed": false
      }
    }
  |]

editIntegrationEventOut1 =
  [aesonQQ|
    {
      "annotations": {
        "changed": true,
        "value": {
          "test": "testValue"
        }
      },
      "requestUrl": {
        "changed": true,
        "value": "https://query.wikidata.org/sparql?example"
      },
      "uuid": "d59d4a54-eb95-401d-8b74-55e8b4742943",
      "responseItemId": {
        "changed": false
      },
      "entityUuid": "b2ae610d-8dd1-4c4d-bf78-5a3c9f00e5d8",
      "props": {
        "changed": false
      },
      "parentUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
      "eventType": "EditIntegrationEvent",
      "name": {
        "changed": true,
        "value": "Wikidata"
      },
      "requestHeaders": {
        "changed": true,
        "value": {
          "testHeader": "testHeaderValue"
        }
      },
      "id": {
        "changed": false
      },
      "responseItemUrl": {
        "changed": false
      },
      "responseListField": {
        "changed": false
      },
      "logo": {
        "changed": false
      },
      "responseItemTemplate": {
        "changed": true,
        "value": "{{item.label.value}}"
      },
      "requestMethod": {
        "changed": false
      },
      "requestBody": {
        "changed": false
      }
    }
  |]
