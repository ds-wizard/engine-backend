module Wizard.Database.Mapping.App.App where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Model.App.App

instance ToRow App where
  toRow App {..} =
    [ toField uuid
    , toField appId
    , toField name
    , toField serverDomain
    , toField clientUrl
    , toField enabled
    , toField createdAt
    , toField updatedAt
    , toField serverUrl
    ]

instance FromRow App where
  fromRow = do
    uuid <- field
    appId <- field
    name <- field
    serverDomain <- field
    clientUrl <- field
    enabled <- field
    createdAt <- field
    updatedAt <- field
    serverUrl <- field
    return $ App {..}
