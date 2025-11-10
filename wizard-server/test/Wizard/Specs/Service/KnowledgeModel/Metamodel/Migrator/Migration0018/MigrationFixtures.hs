module Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0018.MigrationFixtures where

import Data.Aeson.QQ

oldEvent =
  [aesonQQ|
    {
      "uuid": "eff6bbfc-8983-4799-8b89-f7a4ea37b611",
      "parentUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
      "entityUuid": "b101f2d0-2476-452d-aa8d-95a41a02b52c",
      "eventType": "AddPhaseEvent",
      "title": "Before Submitting the Proposal",
      "description": null,
      "annotations": [],
      "createdAt": "2018-10-01T00:00:00Z"
    }
  |]

newEvent =
  [aesonQQ|
    {
      "uuid": "eff6bbfc-8983-4799-8b89-f7a4ea37b611",
      "parentUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
      "entityUuid": "b101f2d0-2476-452d-aa8d-95a41a02b52c",
      "content": {
        "eventType": "AddPhaseEvent",
        "title": "Before Submitting the Proposal",
        "description": null,
        "annotations": []
      },
      "createdAt": "2018-10-01T00:00:00Z"
    }
  |]
