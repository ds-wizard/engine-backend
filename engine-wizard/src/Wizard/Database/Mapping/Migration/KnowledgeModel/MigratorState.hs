module Wizard.Database.Mapping.Migration.KnowledgeModel.MigratorState where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Api.Resource.Event.EventJM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateJM ()
import Wizard.Model.Migration.KnowledgeModel.MigratorState

instance ToRow MigratorState where
  toRow MigratorState {..} =
    [ toField _migratorStateBranchUuid
    , toField _migratorStateMetamodelVersion
    , toJSONField _migratorStateMigrationState
    , toField _migratorStateBranchPreviousPackageId
    , toField _migratorStateTargetPackageId
    , toJSONField _migratorStateBranchEvents
    , toJSONField _migratorStateTargetPackageEvents
    , toJSONField _migratorStateResultEvents
    , toJSONField _migratorStateCurrentKnowledgeModel
    , toField _migratorStateAppUuid
    , toField _migratorStateCreatedAt
    ]

instance FromRow MigratorState where
  fromRow = do
    _migratorStateBranchUuid <- field
    _migratorStateMetamodelVersion <- field
    _migratorStateMigrationState <- fieldWith fromJSONField
    _migratorStateBranchPreviousPackageId <- field
    _migratorStateTargetPackageId <- field
    _migratorStateBranchEvents <- fieldWith fromJSONField
    _migratorStateTargetPackageEvents <- fieldWith fromJSONField
    _migratorStateResultEvents <- fieldWith fromJSONField
    _migratorStateCurrentKnowledgeModel <- fieldWith fromJSONField
    _migratorStateAppUuid <- field
    _migratorStateCreatedAt <- field
    return $ MigratorState {..}
