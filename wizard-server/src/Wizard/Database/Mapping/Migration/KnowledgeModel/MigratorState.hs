module Wizard.Database.Mapping.Migration.KnowledgeModel.MigratorState where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateJM ()
import Wizard.Model.Migration.KnowledgeModel.MigratorState
import WizardLib.KnowledgeModel.Api.Resource.Event.EventJM ()

instance ToRow MigratorState where
  toRow MigratorState {..} =
    [ toField branchUuid
    , toField metamodelVersion
    , toJSONField migrationState
    , toField branchPreviousPackageId
    , toField targetPackageId
    , toJSONField branchEvents
    , toJSONField targetPackageEvents
    , toJSONField resultEvents
    , toJSONField currentKnowledgeModel
    , toField appUuid
    , toField createdAt
    ]

instance FromRow MigratorState where
  fromRow = do
    branchUuid <- field
    metamodelVersion <- field
    migrationState <- fieldWith fromJSONField
    branchPreviousPackageId <- field
    targetPackageId <- field
    branchEvents <- fieldWith fromJSONField
    targetPackageEvents <- fieldWith fromJSONField
    resultEvents <- fieldWith fromJSONField
    currentKnowledgeModel <- fieldWith fromJSONField
    appUuid <- field
    createdAt <- field
    return $ MigratorState {..}
