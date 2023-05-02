module Wizard.Database.Mapping.Branch.Branch where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Model.Branch.Branch

instance FromRow Branch where
  fromRow = do
    uuid <- field
    name <- field
    kmId <- field
    previousPackageId <- field
    createdBy <- field
    createdAt <- field
    updatedAt <- field
    appUuid <- field
    version <- field
    description <- field
    readme <- field
    license <- field
    return $ Branch {..}

instance ToRow Branch where
  toRow Branch {..} =
    [ toField uuid
    , toField name
    , toField kmId
    , toField previousPackageId
    , toField createdBy
    , toField createdAt
    , toField updatedAt
    , toField appUuid
    , toField version
    , toField description
    , toField readme
    , toField license
    ]
