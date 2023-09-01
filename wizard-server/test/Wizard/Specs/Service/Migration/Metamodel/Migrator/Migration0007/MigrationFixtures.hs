module Wizard.Specs.Service.Migration.Metamodel.Migrator.Migration0007.MigrationFixtures where

import Data.Aeson.QQ

addKmEventIn1 =
  [aesonQQ|
    {
      "uuid": "891304e7-8a1d-43e7-8419-3a3b76a2ef56",
      "entityUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
      "parentUuid": "00000000-0000-0000-0000-000000000000",
      "eventType": "AddKnowledgeModelEvent"
    }
  |]

addKmEventOut1 =
  [aesonQQ|
    [
     {
        "uuid": "891304e7-8a1d-43e7-8419-3a3b76a2ef56",
        "entityUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
        "parentUuid": "00000000-0000-0000-0000-000000000000",
        "eventType": "AddKnowledgeModelEvent"
      },
      {
        "uuid": "eff6bbfc-8983-4799-8b89-f7a4ea37b611",
        "entityUuid": "b101f2d0-2476-452d-aa8d-95a41a02b52c",
        "parentUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
        "eventType": "AddPhaseEvent",
        "title": "Before Submitting the Proposal",
        "description": null
      },
      {
        "uuid": "e9bfcd64-411e-424a-98c9-5b4d531f0890",
        "entityUuid": "1796fa3c-9f53-475f-89ff-c66a0453c42e",
        "parentUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
        "eventType": "AddPhaseEvent",
        "title": "Before Submitting the DMP",
        "description": null
      },
      {
        "uuid": "bd22e504-295a-4ade-8fca-7f86aacddd76",
        "entityUuid": "adc9133d-afcd-4616-9aea-db5f475898a2",
        "parentUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
        "eventType": "AddPhaseEvent",
        "title": "Before Finishing the Project",
        "description": null
      },
      {
        "uuid": "fc62249c-700a-45d2-97b8-e5817468a5d5",
        "entityUuid": "1ace0fc6-a949-495f-a32e-e948f3f6bed1",
        "parentUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
        "eventType": "AddPhaseEvent",
        "title": "After Finishing the Project",
        "description": null
      },
      {
        "uuid": "b630056c-3acf-4f26-b317-50f1805402c0",
        "entityUuid": "8db30660-d4e5-4c0a-bf3e-553f3f0f997a",
        "parentUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
        "abbreviation": "F",
        "eventType": "AddMetricEvent",
        "title": "Findability",
        "description": "The Findability metric describes how easily data can be located. The score associated with an answer will be higher if it makes it easier for humans or for computers to locate your data set, e.g. if it ends up in an index or has a unique resolvable identifier."
      },
      {
        "uuid": "c19feec6-2ab2-4757-893a-c11c47f352b8",
        "entityUuid": "0feac7e6-add4-4723-abae-be5ce7864c63",
        "parentUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
        "abbreviation": "A",
        "eventType": "AddMetricEvent",
        "title": "Accessibility",
        "description": "The Accessibility metric describes how well the access to the database is described and how easy it is to implement. The score associated with an answer will be higher if it makes it easier for humans and computers to get to the data. This is determined by e.g. the protocol for accessing the data or for authenticating users, and also by the guaranteed longevity of the repository. Note that this is different from the Openness metric!"
      },
      {
        "uuid": "412854a9-ee08-4630-8e9a-71978c8290b3",
        "entityUuid": "a42bded3-a085-45f8-b384-32b4a77c8385",
        "parentUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
        "abbreviation": "I",
        "eventType": "AddMetricEvent",
        "title": "Interoperability",
        "description": "The Interoperability metric describes how well the data interoperates with other data. The score associated with an answer will be higher if it makes it easier for humans and computers to couple the data with other data and 'understand' relationships. This is influenced by the use of standard ontologies for different fields and proper descriptions of the relations. It is also influenced by proper standard metadata that is agreed by the community."
      },
      {
        "uuid": "b0bf9ac7-cd4c-4e77-9d56-2363e558bc8e",
        "entityUuid": "0bafe0c2-a8f2-4c74-80c8-dbf3a5b8e9b7",
        "parentUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
        "abbreviation": "R",
        "eventType": "AddMetricEvent",
        "title": "Reusability",
        "description": "The Reusability metric describes how well the data is suitable for reuse in other context. The score associated with an answer will be higher if it makes it easier for humans and computers to reuse the data. This is influenced largely by proper description of how the data was obtained, and also by the conditions that are put on the reuse (license and, for personally identifying information, consent)."
      },
      {
        "uuid": "c99e578d-f4cc-4a36-94cc-1451901f11fe",
        "entityUuid": "8845fe2b-79df-4138-baea-3a035bf5e249",
        "parentUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
        "abbreviation": "G",
        "eventType": "AddMetricEvent",
        "title": "Good DMP Practice",
        "description": "The Good DMP Practice metric describes how appreciated a process is among Data Stewards. A score associated with an answer will be high if a practice would be considered preferable over alternatives, generally a good idea."
      },
      {
        "uuid": "04d738dd-3b71-4433-afd6-65b105fa71cd",
        "entityUuid": "cc02c5a0-9754-4432-a7e0-ce0f3cf7a0a0",
        "parentUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
        "abbreviation": "O",
        "eventType": "AddMetricEvent",
        "title": "Openness",
        "description": "The Openness metric describes how Open the data are available. Note that this is different from the Accessibility metric. A score associated with an answer will be high if the data will be as open as possible, and low if voluntary restrictions apply to access and re-use."
      }
    ]
  |]

editKmEventIn1 =
  [aesonQQ|
    {
      "tagUuids": {
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
      "integrationUuids": {
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

addQuestionEventIn0 =
  [aesonQQ|
    {
      "tagUuids": [],
      "text": "Each person contributing to creating or executing the data management plan should be added as a contributor",
      "uuid": "2b815a2a-12a5-4720-8025-c5492969f7f5",
      "entityUuid": "73d686bd-7939-412e-8631-502ee6d9ea7b",
      "questionType": "ListQuestion",
      "parentUuid": "1e85da40-bbfc-4180-903e-6c569ed2da38",
      "eventType": "AddQuestionEvent",
      "title": "Contributor",
      "requiredLevel": null
    }
  |]

addQuestionEventOut0 =
  [aesonQQ|
    {
      "tagUuids": [],
      "text": "Each person contributing to creating or executing the data management plan should be added as a contributor",
      "uuid": "2b815a2a-12a5-4720-8025-c5492969f7f5",
      "entityUuid": "73d686bd-7939-412e-8631-502ee6d9ea7b",
      "questionType": "ListQuestion",
      "parentUuid": "1e85da40-bbfc-4180-903e-6c569ed2da38",
      "eventType": "AddQuestionEvent",
      "title": "Contributor",
      "requiredPhaseUuid": null
    }
  |]

addQuestionEventIn1 =
  [aesonQQ|
    {
      "tagUuids": [],
      "text": "Each person contributing to creating or executing the data management plan should be added as a contributor",
      "uuid": "2b815a2a-12a5-4720-8025-c5492969f7f5",
      "entityUuid": "73d686bd-7939-412e-8631-502ee6d9ea7b",
      "questionType": "ListQuestion",
      "parentUuid": "1e85da40-bbfc-4180-903e-6c569ed2da38",
      "eventType": "AddQuestionEvent",
      "title": "Contributor",
      "requiredLevel": 1
    }
  |]

addQuestionEventOut1 =
  [aesonQQ|
    {
      "tagUuids": [],
      "text": "Each person contributing to creating or executing the data management plan should be added as a contributor",
      "uuid": "2b815a2a-12a5-4720-8025-c5492969f7f5",
      "entityUuid": "73d686bd-7939-412e-8631-502ee6d9ea7b",
      "questionType": "ListQuestion",
      "parentUuid": "1e85da40-bbfc-4180-903e-6c569ed2da38",
      "eventType": "AddQuestionEvent",
      "title": "Contributor",
      "requiredPhaseUuid": "b101f2d0-2476-452d-aa8d-95a41a02b52c"
    }
  |]

addQuestionEventIn2 =
  [aesonQQ|
    {
      "tagUuids": [],
      "text": "Each person contributing to creating or executing the data management plan should be added as a contributor",
      "uuid": "2b815a2a-12a5-4720-8025-c5492969f7f5",
      "entityUuid": "73d686bd-7939-412e-8631-502ee6d9ea7b",
      "questionType": "ListQuestion",
      "parentUuid": "1e85da40-bbfc-4180-903e-6c569ed2da38",
      "eventType": "AddQuestionEvent",
      "title": "Contributor",
      "requiredLevel": 2
    }
  |]

addQuestionEventOut2 =
  [aesonQQ|
    {
      "tagUuids": [],
      "text": "Each person contributing to creating or executing the data management plan should be added as a contributor",
      "uuid": "2b815a2a-12a5-4720-8025-c5492969f7f5",
      "entityUuid": "73d686bd-7939-412e-8631-502ee6d9ea7b",
      "questionType": "ListQuestion",
      "parentUuid": "1e85da40-bbfc-4180-903e-6c569ed2da38",
      "eventType": "AddQuestionEvent",
      "title": "Contributor",
      "requiredPhaseUuid": "1796fa3c-9f53-475f-89ff-c66a0453c42e"
    }
  |]

editQuestionEventIn0 =
  [aesonQQ|
    {
      "tagUuids": {
        "changed": false
      },
      "text": {
        "changed": false
      },
      "uuid": "dee73b95-f66d-4e00-973e-722260b7f789",
      "expertUuids": {
        "changed": false
      },
      "entityUuid": "3a2ffc13-6a0e-4976-bb34-14ab6d938348",
      "questionType": "ValueQuestion",
      "parentUuid": "73d686bd-7939-412e-8631-502ee6d9ea7b",
      "eventType": "EditQuestionEvent",
      "referenceUuids": {
        "changed": false
      },
      "title": {
        "changed": true,
        "value": "E-mail address"
      },
      "valueType": {
        "changed": false
      },
      "requiredLevel": {
        "changed": true,
        "value": null
      }
    }
  |]

editQuestionEventOut0 =
  [aesonQQ|
    {
      "tagUuids": {
        "changed": false
      },
      "text": {
        "changed": false
      },
      "uuid": "dee73b95-f66d-4e00-973e-722260b7f789",
      "expertUuids": {
        "changed": false
      },
      "entityUuid": "3a2ffc13-6a0e-4976-bb34-14ab6d938348",
      "questionType": "ValueQuestion",
      "parentUuid": "73d686bd-7939-412e-8631-502ee6d9ea7b",
      "eventType": "EditQuestionEvent",
      "referenceUuids": {
        "changed": false
      },
      "title": {
        "changed": true,
        "value": "E-mail address"
      },
      "valueType": {
        "changed": false
      },
      "requiredPhaseUuid": {
        "changed": true,
        "value": null
      }
    }
  |]

editQuestionEventIn3 =
  [aesonQQ|
    {
      "tagUuids": {
        "changed": false
      },
      "text": {
        "changed": false
      },
      "uuid": "dee73b95-f66d-4e00-973e-722260b7f789",
      "expertUuids": {
        "changed": false
      },
      "entityUuid": "3a2ffc13-6a0e-4976-bb34-14ab6d938348",
      "questionType": "ValueQuestion",
      "parentUuid": "73d686bd-7939-412e-8631-502ee6d9ea7b",
      "eventType": "EditQuestionEvent",
      "referenceUuids": {
        "changed": false
      },
      "title": {
        "changed": true,
        "value": "E-mail address"
      },
      "valueType": {
        "changed": false
      },
      "requiredLevel": {
        "changed": true,
        "value": 3
      }
    }
  |]

editQuestionEventOut3 =
  [aesonQQ|
    {
      "tagUuids": {
        "changed": false
      },
      "text": {
        "changed": false
      },
      "uuid": "dee73b95-f66d-4e00-973e-722260b7f789",
      "expertUuids": {
        "changed": false
      },
      "entityUuid": "3a2ffc13-6a0e-4976-bb34-14ab6d938348",
      "questionType": "ValueQuestion",
      "parentUuid": "73d686bd-7939-412e-8631-502ee6d9ea7b",
      "eventType": "EditQuestionEvent",
      "referenceUuids": {
        "changed": false
      },
      "title": {
        "changed": true,
        "value": "E-mail address"
      },
      "valueType": {
        "changed": false
      },
      "requiredPhaseUuid": {
        "changed": true,
        "value": "adc9133d-afcd-4616-9aea-db5f475898a2"
      }
    }
  |]
