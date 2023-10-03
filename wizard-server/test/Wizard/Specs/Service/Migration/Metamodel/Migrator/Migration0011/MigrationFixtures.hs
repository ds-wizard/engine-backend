module Wizard.Specs.Service.Migration.Metamodel.Migrator.Migration0011.MigrationFixtures where

import Data.Aeson.QQ

addIntegrationEventIn1 =
  [aesonQQ|
    {
      "annotations": [],
      "requestUrl": "${apiUrl}/search/?q=${q}&registry=${registry}",
      "createdAt": "2019-06-11T05:39:31.678Z",
      "uuid": "82090e7d-7aa8-4339-a8c5-2330c27ae147",
      "responseItemId": "{{item.record_id}}",
      "entityUuid": "f3520558-707e-415a-8878-c7b8d6ccc2fb",
      "props": [
        "registry"
      ],
      "parentUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
      "eventType": "AddIntegrationEvent",
      "name": "FAIRsharing",
      "requestHeaders": [
        {
          "value": "application/json",
          "key": "Accept"
        },
        {
          "value": "${apiKey}",
          "key": "Api-Key"
        }
      ],
      "id": "fairsharing",
      "responseItemUrl": "https://fairsharing.org/${id}",
      "responseListField": "results",
      "logo": "exampleLogo",
      "responseItemTemplate": "{{item.name}}",
      "requestMethod": "GET",
      "requestBody": ""
    }
  |]

addIntegrationEventOut1 =
  [aesonQQ|
    {
      "annotations": [],
      "requestUrl": "${apiUrl}/search/?q=${q}&registry=${registry}",
      "createdAt": "2019-06-11T05:39:31.678Z",
      "entityUuid": "f3520558-707e-415a-8878-c7b8d6ccc2fb",
      "props": [
        "registry"
      ],
      "eventType": "AddIntegrationEvent",
      "name": "FAIRsharing",
      "id": "fairsharing",
      "requestHeaders": [
        {
          "value": "application/json",
          "key": "Accept"
        },
        {
          "value": "${apiKey}",
          "key": "Api-Key"
        }
      ],
      "responseListField": "results",
      "integrationType": "ApiIntegration",
      "requestBody": "",
      "uuid": "82090e7d-7aa8-4339-a8c5-2330c27ae147",
      "responseItemId": "{{item.record_id}}",
      "requestEmptySearch": true,
      "parentUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
      "logo": "exampleLogo",
      "responseItemTemplate": "{{item.name}}",
      "itemUrl": "https://fairsharing.org/${id}",
      "requestMethod": "GET"
    }
  |]

editIntegrationEventIn1 =
  [aesonQQ|
    {
      "annotations": {
        "changed": false
      },
      "requestUrl": {
        "changed": true,
        "value": "https://query.wikidata.org/sparql?example"
      },
      "createdAt": "2020-11-11T10:49:19.845Z",
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
        "changed": false
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

editIntegrationEventOut1 =
  [aesonQQ|
    {
      "annotations": {
        "changed": false
      },
      "requestUrl": {
        "changed": true,
        "value": "https://query.wikidata.org/sparql?example"
      },
      "createdAt": "2020-11-11T10:49:19.845Z",
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
      "integrationType": "ApiIntegration",
      "name": {
        "changed": true,
        "value": "Wikidata"
      },
      "requestEmptySearch": {
        "changed": false
      },
      "requestHeaders": {
        "changed": false
      },
      "id": {
        "changed": false
      },
      "itemUrl": {
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
