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
    [ toField branchUuid
    , toField metamodelVersion
    , toJSONField events
    , toField appUuid
    , toField createdAt
    , toField updatedAt
    ]

instance FromRow BranchData where
  fromRow = do
    branchUuid <- field
    metamodelVersion <- field
    events <- fieldWith fromJSONField
    appUuid <- field
    createdAt <- field
    updatedAt <- field
    return $ BranchData {..}
