module Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0016.MigrationFixtures where

import Data.Aeson.QQ

eventIn0 =
  [aesonQQ|
    {
      "annotations": [],
      "createdAt": "2024-11-25T10:05:44.836Z",
      "entityUuid": "49817a48-f7a4-4a19-af35-d724c11029b8",
      "eventType": "AddQuestionEvent",
      "parentUuid": "ce31a219-852b-49fd-a052-44a2ab803844",
      "questionType": "ValueQuestion",
      "requiredPhaseUuid": null,
      "tagUuids": [],
      "text": null,
      "title": "",
      "uuid": "23408471-d30b-4d12-965a-982169c86952"
    }
  |]

eventOut0 =
  [aesonQQ|
    {
      "annotations": [],
      "createdAt": "2024-11-25T10:05:44.836Z",
      "entityUuid": "49817a48-f7a4-4a19-af35-d724c11029b8",
      "eventType": "AddQuestionEvent",
      "parentUuid": "ce31a219-852b-49fd-a052-44a2ab803844",
      "questionType": "ValueQuestion",
      "requiredPhaseUuid": null,
      "tagUuids": [],
      "text": null,
      "title": "",
      "uuid": "23408471-d30b-4d12-965a-982169c86952",
      "validations": []
    }
  |]

eventIn1 =
  [aesonQQ|
    {
      "annotations": {
        "changed": false
      },
      "createdAt": "2024-11-25T10:05:47.861Z",
      "entityUuid": "49817a48-f7a4-4a19-af35-d724c11029b8",
      "eventType": "EditQuestionEvent",
      "expertUuids": {
        "changed": false
      },
      "parentUuid": "ce31a219-852b-49fd-a052-44a2ab803844",
      "questionType": "ValueQuestion",
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
        "value": "Value question ABC"
      },
      "uuid": "6ba428de-6b07-4a78-9d15-64d4726632c5",
      "valueType": {
        "changed": true,
        "value": "StringQuestionValueType"
      }
    }
  |]

eventOut1 =
  [aesonQQ|
    {
      "annotations": {
        "changed": false
      },
      "createdAt": "2024-11-25T10:05:47.861Z",
      "entityUuid": "49817a48-f7a4-4a19-af35-d724c11029b8",
      "eventType": "EditQuestionEvent",
      "expertUuids": {
        "changed": false
      },
      "parentUuid": "ce31a219-852b-49fd-a052-44a2ab803844",
      "questionType": "ValueQuestion",
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
        "value": "Value question ABC"
      },
      "uuid": "6ba428de-6b07-4a78-9d15-64d4726632c5",
      "validations": {
        "changed": false
      },
      "valueType": {
        "changed": true,
        "value": "StringQuestionValueType"
      }
    }
  |]

eventIn2 =
  [aesonQQ|
    {
      "annotations": [],
      "createdAt": "2024-11-25T10:06:28.103Z",
      "entityUuid": "2632abe8-7a9a-4a13-bd70-9c6880d2be32",
      "eventType": "AddQuestionEvent",
      "parentUuid": "ce31a219-852b-49fd-a052-44a2ab803844",
      "questionType": "OptionsQuestion",
      "requiredPhaseUuid": null,
      "tagUuids": [],
      "text": null,
      "title": "",
      "uuid": "92bdb694-177b-44aa-96f4-f3d4369ee20c"
    }
  |]

eventOut2 = eventIn2

eventIn3 =
  [aesonQQ|
    {
      "annotations": {
        "changed": false
      },
      "createdAt": "2024-11-25T10:06:31.299Z",
      "entityUuid": "2632abe8-7a9a-4a13-bd70-9c6880d2be32",
      "eventType": "EditQuestionEvent",
      "expertUuids": {
        "changed": false
      },
      "fileTypes": {
        "changed": false
      },
      "maxSize": {
        "changed": false
      },
      "parentUuid": "ce31a219-852b-49fd-a052-44a2ab803844",
      "questionType": "FileQuestion",
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
        "value": "Test file"
      },
      "uuid": "68105db3-f3b1-4f09-92f4-e508058cf6a6"
    }
  |]

eventOut3 = eventIn3
