module Wizard.Database.Mapping.Branch.Branch where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Model.Branch.Branch

instance FromRow Branch where
  fromRow = do
    _branchUuid <- field
    _branchName <- field
    _branchKmId <- field
    _branchPreviousPackageId <- field
    _branchCreatedBy <- field
    _branchCreatedAt <- field
    _branchUpdatedAt <- field
    _branchAppUuid <- field
    return $ Branch {..}

instance ToRow Branch where
  toRow Branch {..} =
    [ toField _branchUuid
    , toField _branchName
    , toField _branchKmId
    , toField _branchPreviousPackageId
    , toField _branchCreatedBy
    , toField _branchCreatedAt
    , toField _branchUpdatedAt
    , toField _branchAppUuid
    ]
