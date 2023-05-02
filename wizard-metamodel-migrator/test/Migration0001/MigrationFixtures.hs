module Migration0001.MigrationFixtures where

import Data.Aeson.QQ


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
      "entityUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
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
      "integrationUuids": {
        "changed": false
      }
    }
  |]
