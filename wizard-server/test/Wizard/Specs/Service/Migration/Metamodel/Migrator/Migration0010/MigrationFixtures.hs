module Wizard.Specs.Service.Migration.Metamodel.Migrator.Migration0010.MigrationFixtures where

import Data.Aeson.QQ

addPhaseEventIn1 =
  [aesonQQ|
    {
      "annotations": {},
      "uuid": "eff6bbfc-8983-4799-8b89-f7a4ea37b611",
      "entityUuid": "b101f2d0-2476-452d-aa8d-95a41a02b52c",
      "parentUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
      "eventType": "AddPhaseEvent",
      "title": "Before Submitting the Proposal",
      "description": null
    }
  |]

addPhaseEventOut1 =
  [aesonQQ|
    {
      "annotations": [],
      "createdAt": "2022-01-01T12:00:00Z",
      "uuid": "eff6bbfc-8983-4799-8b89-f7a4ea37b611",
      "entityUuid": "b101f2d0-2476-452d-aa8d-95a41a02b52c",
      "parentUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
      "eventType": "AddPhaseEvent",
      "title": "Before Submitting the Proposal",
      "description": null
    }
  |]

deleteQuestionEventIn1 =
  [aesonQQ|
    {
      "uuid": "0fcdd1bb-010f-4b15-9d57-54d8343ac020",
      "entityUuid": "8d082e2f-7792-4ef7-a851-6e5041cba8b6",
      "parentUuid": "b1df3c74-0b1f-4574-81c4-4cc2d780c1af",
      "eventType": "DeleteQuestionEvent"
    }
  |]

deleteQuestionEventOut1 =
  [aesonQQ|
    {
      "createdAt": "2022-01-01T12:00:00Z",
      "uuid": "0fcdd1bb-010f-4b15-9d57-54d8343ac020",
      "entityUuid": "8d082e2f-7792-4ef7-a851-6e5041cba8b6",
      "parentUuid": "b1df3c74-0b1f-4574-81c4-4cc2d780c1af",
      "eventType": "DeleteQuestionEvent"
    }
  |]

editKmEventIn1 =
  [aesonQQ|
    {
      "annotations": {
        "changed": true,
        "value": {
          "akey": "avalue"
        }
      },
      "tagUuids": {
        "changed": false
      },
      "uuid": "57bf29d7-d74d-4da3-8d28-f49855432d88",
      "entityUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
      "parentUuid": "00000000-0000-0000-0000-000000000000",
      "eventType": "EditKnowledgeModelEvent",
      "chapterUuids": {
        "changed": true,
        "value": [
          "1e85da40-bbfc-4180-903e-6c569ed2da38",
          "82fd0cce-2b41-423f-92ad-636d0872045c"
        ]
      },
      "metricUuids": {
        "changed": false
      },
      "integrationUuids": {
        "changed": false
      },
      "phaseUuids": {
        "changed": false
      }
    }
  |]

editKmEventOut1 =
  [aesonQQ|
    {
      "annotations": {
        "changed": true,
        "value": [
          {
            "key": "akey",
            "value": "avalue"
          }
        ]
      },
      "tagUuids": {
        "changed": false
      },
      "createdAt": "2022-01-01T12:00:00Z",
      "uuid": "57bf29d7-d74d-4da3-8d28-f49855432d88",
      "entityUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
      "parentUuid": "00000000-0000-0000-0000-000000000000",
      "eventType": "EditKnowledgeModelEvent",
      "chapterUuids": {
        "changed": true,
        "value": [
          "1e85da40-bbfc-4180-903e-6c569ed2da38",
          "82fd0cce-2b41-423f-92ad-636d0872045c"
        ]
      },
      "metricUuids": {
        "changed": false
      },
      "integrationUuids": {
        "changed": false
      },
      "phaseUuids": {
        "changed": false
      }
    }
  |]

addIntegrationEventIn1 =
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

addIntegrationEventOut1 =
  [aesonQQ|
    {
      "annotations": [
        {
          "value": "value",
          "key": "test"
        }
      ],
      "requestUrl": "${apiUrl}/search/?q=${q}&registry=${registry}",
      "createdAt": "2022-01-01T12:00:00Z",
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

editIntegrationEventOut1 =
  [aesonQQ|
    {
      "annotations": {
        "changed": true,
        "value": [
          {
            "key": "test",
            "value": "testValue"
          }
        ]
      },
      "requestUrl": {
        "changed": true,
        "value": "https://query.wikidata.org/sparql?example"
      },
      "createdAt": "2022-01-01T12:00:00Z",
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
        "value": [
          {
            "key": "testHeader",
            "value": "testHeaderValue"
          }
        ]
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
