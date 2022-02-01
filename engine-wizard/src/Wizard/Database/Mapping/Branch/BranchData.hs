module Wizard.Database.Mapping.Branch.BranchData where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Api.Resource.Event.EventJM ()
import Wizard.Model.Branch.BranchData

instance ToRow BranchData where
  toRow BranchData {..} =
    [ toField _branchDataBranchUuid
    , toField _branchDataMetamodelVersion
    , toJSONField _branchDataEvents
    , toField _branchDataAppUuid
    , toField _branchDataCreatedAt
    , toField _branchDataUpdatedAt
    ]

instance FromRow BranchData where
  fromRow = do
    _branchDataBranchUuid <- field
    _branchDataMetamodelVersion <- field
    _branchDataEvents <- fieldWith fromJSONField
    _branchDataAppUuid <- field
    _branchDataCreatedAt <- field
    _branchDataUpdatedAt <- field
    return $ BranchData {..}
