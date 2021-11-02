module Wizard.Database.Mapping.Branch.Branch where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow

import Shared.Api.Resource.Event.EventJM ()
import Shared.Model.Event.Event
import Wizard.Model.Branch.Branch

instance FromRow Branch where
  fromRow = do
    _branchUuid <- field
    _branchName <- field
    _branchKmId <- field
    _branchMetamodelVersion <- field
    _branchPreviousPackageId <- field
    _ <- fieldWith fromJSONField :: RowParser [Event]
    _branchOwnerUuid <- field
    _branchCreatedAt <- field
    _branchUpdatedAt <- field
    _branchAppUuid <- field
    return $ Branch {..}
