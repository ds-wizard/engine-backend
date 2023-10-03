module Wizard.Specs.Service.Migration.Metamodel.Migrator.Migration0003.MigrationFixtures where

import Data.Aeson.QQ

addKmEventIn1 =
  [aesonQQ|
    {
      "uuid": "891304e7-8a1d-43e7-8419-3a3b76a2ef56",
      "kmUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
      "path": [],
      "eventType": "AddKnowledgeModelEvent",
      "name": "My test KM"
    }
  |]

addKmEventOut1 =
  [aesonQQ|
    {
      "uuid": "891304e7-8a1d-43e7-8419-3a3b76a2ef56",
      "entityUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
      "parentUuid": "00000000-0000-0000-0000-000000000000",
      "eventType": "AddKnowledgeModelEvent",
      "name": "My test KM"
    }
  |]

editKmEventIn1 =
  [aesonQQ|
    {
      "tagUuids": {
        "changed": false
      },
      "name": {
        "changed": false
      },
      "uuid": "d402825b-b145-4ad3-8578-895dfbd312c1",
      "kmUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
      "path": [],
      "eventType": "EditKnowledgeModelEvent",
      "chapterUuids": {
        "changed": true,
        "value": [
          "1e85da40-bbfc-4180-903e-6c569ed2da38",
          "82fd0cce-2b41-423f-92ad-636d0872045c",
          "d5b27482-b598-4b8c-b534-417d4ad27394",
          "6be88f7c-f868-460f-bba7-91e1c659adfd"
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
      "tagUuids": {
        "changed": false
      },
      "name": {
        "changed": false
      },
      "uuid": "d402825b-b145-4ad3-8578-895dfbd312c1",
      "entityUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
      "parentUuid": "00000000-0000-0000-0000-000000000000",
      "eventType": "EditKnowledgeModelEvent",
      "chapterUuids": {
        "changed": true,
        "value": [
          "1e85da40-bbfc-4180-903e-6c569ed2da38",
          "82fd0cce-2b41-423f-92ad-636d0872045c",
          "d5b27482-b598-4b8c-b534-417d4ad27394",
          "6be88f7c-f868-460f-bba7-91e1c659adfd"
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

deleteQuestionEventIn1 =
  [aesonQQ|
    {
      "uuid": "0fcdd1bb-010f-4b15-9d57-54d8343ac020",
      "questionUuid": "8d082e2f-7792-4ef7-a851-6e5041cba8b6",
      "path": [
        {
          "type": "chapter",
          "uuid": "something1"
        },
        {
          "type": "question",
          "uuid": "something2"
        },
        {
          "type": "answer",
          "uuid": "b1df3c74-0b1f-4574-81c4-4cc2d780c1af"
        }
      ],
      "eventType": "DeleteQuestionEvent"
    }
  |]

deleteQuestionEventOut1 =
  [aesonQQ|
    {
      "uuid": "0fcdd1bb-010f-4b15-9d57-54d8343ac020",
      "entityUuid": "8d082e2f-7792-4ef7-a851-6e5041cba8b6",
      "parentUuid": "b1df3c74-0b1f-4574-81c4-4cc2d780c1af",
      "eventType": "DeleteQuestionEvent"
    }
  |]
