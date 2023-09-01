module Wizard.Specs.Service.Migration.Metamodel.Migrator.Migration0005.MigrationFixtures where

import Data.Aeson.QQ

addKmEventIn1 =
  [aesonQQ|
    {
      "uuid": "891304e7-8a1d-43e7-8419-3a3b76a2ef56",
      "entityUuid": "6f6241d9-136c-4770-9d58-8a3f39607052",
      "parentUuid": "00000000-0000-0000-0000-000000000000",
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
