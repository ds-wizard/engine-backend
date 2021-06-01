module Wizard.Database.Mapping.Branch.BranchWithEvents where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Api.Resource.Event.EventJM ()
import Wizard.Model.Branch.Branch

instance ToRow BranchWithEvents where
  toRow BranchWithEvents {..} =
    [ toField _branchWithEventsUuid
    , toField _branchWithEventsName
    , toField _branchWithEventsKmId
    , toField _branchWithEventsMetamodelVersion
    , toField _branchWithEventsPreviousPackageId
    , toJSONField _branchWithEventsEvents
    , toField _branchWithEventsOwnerUuid
    , toField _branchWithEventsCreatedAt
    , toField _branchWithEventsUpdatedAt
    ]

instance FromRow BranchWithEvents where
  fromRow = do
    _branchWithEventsUuid <- field
    _branchWithEventsName <- field
    _branchWithEventsKmId <- field
    _branchWithEventsMetamodelVersion <- field
    _branchWithEventsPreviousPackageId <- field
    _branchWithEventsEvents <- fieldWith fromJSONField
    _branchWithEventsOwnerUuid <- field
    _branchWithEventsCreatedAt <- field
    _branchWithEventsUpdatedAt <- field
    return $ BranchWithEvents {..}
