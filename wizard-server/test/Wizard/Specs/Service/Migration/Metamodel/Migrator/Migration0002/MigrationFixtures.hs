module Wizard.Specs.Service.Migration.Metamodel.Migrator.Migration0002.MigrationFixtures where

import Data.Aeson.QQ

addQuestionEventIn1 =
  [aesonQQ|
    {
      "tagUuids": [],
      "text": null,
      "uuid": "7d20cfe3-2a13-46f1-804d-0239dbfc1dc1",
      "entityUuid": "e24167a2-14ec-445d-b205-fb6c7e24a7d1",
      "questionType": "ValueQuestion",
      "path": [],
      "eventType": "AddQuestionEvent",
      "title": "What other quality processes do you use?",
      "valueType": "TextValue",
      "requiredLevel": null
    }
  |]

addQuestionEventOut1 =
  [aesonQQ|
    {
      "tagUuids": [],
      "text": null,
      "uuid": "7d20cfe3-2a13-46f1-804d-0239dbfc1dc1",
      "entityUuid": "e24167a2-14ec-445d-b205-fb6c7e24a7d1",
      "questionType": "ValueQuestion",
      "path": [],
      "eventType": "AddQuestionEvent",
      "title": "What other quality processes do you use?",
      "valueType": "TextQuestionValueType",
      "requiredLevel": null
    }
  |]

addQuestionEventIn2 =
  [aesonQQ|
    {
      "tagUuids": [],
      "text": null,
      "uuid": "7d20cfe3-2a13-46f1-804d-0239dbfc1dc1",
      "entityUuid": "e24167a2-14ec-445d-b205-fb6c7e24a7d1",
      "questionType": "ValueQuestion",
      "path": [],
      "eventType": "AddQuestionEvent",
      "title": "What other quality processes do you use?",
      "valueType": "DateValue",
      "requiredLevel": null
    }
  |]

addQuestionEventOut2 =
  [aesonQQ|
    {
      "tagUuids": [],
      "text": null,
      "uuid": "7d20cfe3-2a13-46f1-804d-0239dbfc1dc1",
      "entityUuid": "e24167a2-14ec-445d-b205-fb6c7e24a7d1",
      "questionType": "ValueQuestion",
      "path": [],
      "eventType": "AddQuestionEvent",
      "title": "What other quality processes do you use?",
      "valueType": "DateQuestionValueType",
      "requiredLevel": null
    }
  |]

editQuestionEventIn1 =
  [aesonQQ|
    {
      "tagUuids": {
        "changed": true,
        "value": [
          "fd4637a2-a117-460a-a7fa-c03760c42629"
        ]
      },
      "text": {
        "changed": false
      },
      "uuid": "c9d177e5-cf44-4634-ba69-2a874ab2e990",
      "expertUuids": {
        "changed": false
      },
      "entityUuid": "448403e9-578b-4155-9017-d4aa9119ea6a",
      "questionType": "ValueQuestion",
      "path": [],
      "eventType": "EditQuestionEvent",
      "referenceUuids": {
        "changed": false
      },
      "title": {
        "changed": false
      },
      "valueType": {
        "changed": true,
        "value": "StringValue"
      },
      "requiredLevel": {
        "changed": false
      }
    }
  |]

editQuestionEventOut1 =
  [aesonQQ|
    {
      "tagUuids": {
        "changed": true,
        "value": [
          "fd4637a2-a117-460a-a7fa-c03760c42629"
        ]
      },
      "text": {
        "changed": false
      },
      "uuid": "c9d177e5-cf44-4634-ba69-2a874ab2e990",
      "expertUuids": {
        "changed": false
      },
      "entityUuid": "448403e9-578b-4155-9017-d4aa9119ea6a",
      "questionType": "ValueQuestion",
      "path": [],
      "eventType": "EditQuestionEvent",
      "referenceUuids": {
        "changed": false
      },
      "title": {
        "changed": false
      },
      "valueType": {
        "changed": true,
        "value": "StringQuestionValueType"
      },
      "requiredLevel": {
        "changed": false
      }
    }
  |]
