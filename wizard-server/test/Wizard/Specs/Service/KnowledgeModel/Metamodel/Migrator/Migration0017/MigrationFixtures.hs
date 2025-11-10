module Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0017.MigrationFixtures where

import Data.Aeson.QQ

addIntegrationEventIn1 =
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

addIntegrationEventOut1 =
  [aesonQQ|
    {
      "annotations": [],
      "requestUrl": "${apiUrl}/search/?q=${q}&registry=${registry}",
      "createdAt": "2019-06-11T05:39:31.678Z",
      "entityUuid": "f3520558-707e-415a-8878-c7b8d6ccc2fb",
      "variables": [
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
      "integrationType": "ApiLegacyIntegration",
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
      "variables": {
        "changed": false
      },
      "parentUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
      "eventType": "EditIntegrationEvent",
      "integrationType": "ApiLegacyIntegration",
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

addQuestionEventIn1 =
  [aesonQQ|
    {
      "annotations": [],
      "createdAt": "2025-07-21T10:28:38.303Z",
      "entityUuid": "de51787b-31ba-4bef-aa3f-55b7d1507419",
      "eventType": "AddQuestionEvent",
      "parentUuid": "80f61c80-6086-4aaf-836a-c753c2ca5662",
      "questionType": "IntegrationQuestion",
      "integrationUuid": "ee3ae0a2-2a53-45e6-97d8-58fc4b87a4f1",
      "props": {},
      "requiredPhaseUuid": null,
      "tagUuids": [],
      "text": null,
      "title": "",
      "uuid": "ee3ae0a2-2a53-45e6-97d8-58fc4b87a4f7"
    }
  |]

addQuestionEventOut1 =
  [aesonQQ|
    {
      "annotations": [],
      "createdAt": "2025-07-21T10:28:38.303Z",
      "entityUuid": "de51787b-31ba-4bef-aa3f-55b7d1507419",
      "eventType": "AddQuestionEvent",
      "parentUuid": "80f61c80-6086-4aaf-836a-c753c2ca5662",
      "questionType": "IntegrationQuestion",
      "integrationUuid": "ee3ae0a2-2a53-45e6-97d8-58fc4b87a4f1",
      "variables": {},
      "requiredPhaseUuid": null,
      "tagUuids": [],
      "text": null,
      "title": "",
      "uuid": "ee3ae0a2-2a53-45e6-97d8-58fc4b87a4f7"
    }
  |]

editQuestionEventIn1 =
  [aesonQQ|
    {
      "annotations": {
        "changed": false
      },
      "createdAt": "2025-07-21T10:28:40.343Z",
      "entityUuid": "de51787b-31ba-4bef-aa3f-55b7d1507419",
      "eventType": "EditQuestionEvent",
      "expertUuids": {
        "changed": false
      },
      "integrationUuid": {
        "changed": true,
        "value": "d502a3c1-ba9c-4579-b996-ae1ee24b6b67"
      },
      "parentUuid": "80f61c80-6086-4aaf-836a-c753c2ca5662",
      "props": {
        "changed": false
      },
      "questionType": "IntegrationQuestion",
      "referenceUuids": {
        "changed": false
      },
      "requiredPhaseUuid": {
        "changed": false
      },
      "tagUuids": {
        "changed": false
      },
      "text": {
        "changed": false
      },
      "title": {
        "changed": true,
        "value": "Integration Question"
      },
      "uuid": "1783fb16-6196-4795-a993-782eecaabcc0"
    }
  |]

editQuestionEventOut1 =
  [aesonQQ|
    {
      "annotations": {
        "changed": false
      },
      "createdAt": "2025-07-21T10:28:40.343Z",
      "entityUuid": "de51787b-31ba-4bef-aa3f-55b7d1507419",
      "eventType": "EditQuestionEvent",
      "expertUuids": {
        "changed": false
      },
      "integrationUuid": {
        "changed": true,
        "value": "d502a3c1-ba9c-4579-b996-ae1ee24b6b67"
      },
      "parentUuid": "80f61c80-6086-4aaf-836a-c753c2ca5662",
      "variables": {
        "changed": false
      },
      "questionType": "IntegrationQuestion",
      "referenceUuids": {
        "changed": false
      },
      "requiredPhaseUuid": {
        "changed": false
      },
      "tagUuids": {
        "changed": false
      },
      "text": {
        "changed": false
      },
      "title": {
        "changed": true,
        "value": "Integration Question"
      },
      "uuid": "1783fb16-6196-4795-a993-782eecaabcc0"
    }
  |]
