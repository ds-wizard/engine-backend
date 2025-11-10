module Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0014.MigrationFixtures where

import Data.Aeson.QQ

eventIn0 =
  [aesonQQ|
    {
      "annotations": [],
      "createdAt": "2018-10-01T00:00:00Z",
      "entityUuid": "e48cb16a-0efc-4ff9-b673-7190176c8b94",
      "eventType": "AddReferenceEvent",
      "parentUuid": "efc80cc8-8318-4f8c-acb7-dc1c60e491c1",
      "referenceType": "ResourcePageReference",
      "shortUuid": "atq",
      "uuid": "010ec99c-23e3-4e75-857f-444c73c20d04"
    }
  |]

eventOut0 =
  [aesonQQ|
    {
      "annotations": [],
      "createdAt": "2018-10-01T00:00:00Z",
      "entityUuid": "e48cb16a-0efc-4ff9-b673-7190176c8b94",
      "eventType": "AddReferenceEvent",
      "parentUuid": "efc80cc8-8318-4f8c-acb7-dc1c60e491c1",
      "referenceType": "ResourcePageReference",
      "resourcePageUuid": null,
      "uuid": "010ec99c-23e3-4e75-857f-444c73c20d04"
    }
  |]

eventIn1 =
  [aesonQQ|
    {
      "annotations": {
          "changed": false
      },
      "createdAt": "2020-01-04T14:33:19.68Z",
      "entityUuid": "bfa19d53-24f3-432c-b12c-675f73bc6c70",
      "eventType": "EditReferenceEvent",
      "parentUuid": "973d38a0-e363-4162-9aed-1803423e85fe",
      "referenceType": "ResourcePageReference",
      "shortUuid": {
        "changed": true,
        "value": "atq"
      },
      "uuid": "cf31cc2e-c663-4de0-9e1f-0697af7e0ce6"
    }
  |]

eventOut1 =
  [aesonQQ|
    {
      "annotations": {
          "changed": false
      },
      "createdAt": "2020-01-04T14:33:19.68Z",
      "entityUuid": "bfa19d53-24f3-432c-b12c-675f73bc6c70",
      "eventType": "EditReferenceEvent",
      "parentUuid": "973d38a0-e363-4162-9aed-1803423e85fe",
      "referenceType": "ResourcePageReference",
      "resourcePageUuid": {
        "changed": false
      },
      "uuid": "cf31cc2e-c663-4de0-9e1f-0697af7e0ce6"
    }
  |]

eventIn2 =
  [aesonQQ|
    {
      "annotations": {
          "changed": false
      },
      "createdAt": "2020-01-04T14:33:19.68Z",
      "entityUuid": "bfa19d53-24f3-432c-b12c-675f73bc6c70",
      "eventType": "EditReferenceEvent",
      "label": {
          "changed": true,
          "value": "Legal bases for personal data processing under GDPR"
      },
      "parentUuid": "973d38a0-e363-4162-9aed-1803423e85fe",
      "referenceType": "URLReference",
      "url": {
          "changed": false
      },
      "uuid": "cf31cc2e-c663-4de0-9e1f-0697af7e0ce6"
    }
  |]

eventOut2 =
  [aesonQQ|
    {
      "annotations": {
          "changed": false
      },
      "createdAt": "2020-01-04T14:33:19.68Z",
      "entityUuid": "bfa19d53-24f3-432c-b12c-675f73bc6c70",
      "eventType": "EditReferenceEvent",
      "label": {
          "changed": true,
          "value": "Legal bases for personal data processing under GDPR"
      },
      "parentUuid": "973d38a0-e363-4162-9aed-1803423e85fe",
      "referenceType": "URLReference",
      "url": {
          "changed": false
      },
      "uuid": "cf31cc2e-c663-4de0-9e1f-0697af7e0ce6"
    }
  |]

eventIn3 =
  [aesonQQ|
    {
      "annotations": [],
      "createdAt": "2018-10-01T00:00:00Z",
      "description": "Keeping a log of every step in the analysis workflow that has been run",
      "entityUuid": "7d7b817f-9f82-4cdf-96a9-565bc67f3129",
      "eventType": "AddReferenceEvent",
      "parentUuid": "daa06d1a-6314-4c6d-93a8-69954c7f5c54",
      "referenceType": "CrossReference",
      "targetUuid": "1991077f-04ae-4808-90a5-e4b2f82e30bf",
      "uuid": "b294bab4-647c-49ac-b967-b086892a7487"
    }
  |]

eventOut3 =
  [aesonQQ|
    {
      "annotations": [],
      "createdAt": "2018-10-01T00:00:00Z",
      "description": "Keeping a log of every step in the analysis workflow that has been run",
      "entityUuid": "7d7b817f-9f82-4cdf-96a9-565bc67f3129",
      "eventType": "AddReferenceEvent",
      "parentUuid": "daa06d1a-6314-4c6d-93a8-69954c7f5c54",
      "referenceType": "CrossReference",
      "targetUuid": "1991077f-04ae-4808-90a5-e4b2f82e30bf",
      "uuid": "b294bab4-647c-49ac-b967-b086892a7487"
    }
  |]

eventIn4 =
  [aesonQQ|
    {
      "annotations": {
          "changed": false
      },
      "chapterUuids": {
          "changed": true,
          "value": [
              "1e85da40-bbfc-4180-903e-6c569ed2da38",
              "82fd0cce-2b41-423f-92ad-636d0872045c",
              "b1df3c74-0b1f-4574-81c4-4cc2d780c1af",
              "c4eda690-066f-495a-8c29-8e8a258ac487",
              "10a10ffd-bfe1-4c6b-bbb6-3dfb1e63a5d5",
              "83438863-0aa0-4458-b14b-2b2c0d4f811d",
              "d5b27482-b598-4b8c-b534-417d4ad27394",
              "6be88f7c-f868-460f-bba7-91e1c659adfd"
          ]
      },
      "createdAt": "2020-01-04T14:33:19.68Z",
      "entityUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
      "eventType": "EditKnowledgeModelEvent",
      "integrationUuids": {
          "changed": false
      },
      "metricUuids": {
          "changed": false
      },
      "parentUuid": "00000000-0000-0000-0000-000000000000",
      "phaseUuids": {
          "changed": false
      },
      "tagUuids": {
          "changed": false
      },
      "uuid": "d402825b-b145-4ad3-8578-895dfbd312c1"
    }
  |]

eventOut4 =
  [aesonQQ|
    {
      "annotations": {
          "changed": false
      },
      "chapterUuids": {
          "changed": true,
          "value": [
              "1e85da40-bbfc-4180-903e-6c569ed2da38",
              "82fd0cce-2b41-423f-92ad-636d0872045c",
              "b1df3c74-0b1f-4574-81c4-4cc2d780c1af",
              "c4eda690-066f-495a-8c29-8e8a258ac487",
              "10a10ffd-bfe1-4c6b-bbb6-3dfb1e63a5d5",
              "83438863-0aa0-4458-b14b-2b2c0d4f811d",
              "d5b27482-b598-4b8c-b534-417d4ad27394",
              "6be88f7c-f868-460f-bba7-91e1c659adfd"
          ]
      },
      "createdAt": "2020-01-04T14:33:19.68Z",
      "entityUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
      "eventType": "EditKnowledgeModelEvent",
      "integrationUuids": {
          "changed": false
      },
      "metricUuids": {
          "changed": false
      },
      "parentUuid": "00000000-0000-0000-0000-000000000000",
      "phaseUuids": {
          "changed": false
      },
      "resourceCollectionUuids": {
          "changed": false
      },
      "tagUuids": {
          "changed": false
      },
      "uuid": "d402825b-b145-4ad3-8578-895dfbd312c1"
    }
  |]
