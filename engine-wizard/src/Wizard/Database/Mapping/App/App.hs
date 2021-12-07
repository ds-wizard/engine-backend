module Wizard.Database.Mapping.App.App where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Model.App.App

instance ToRow App where
  toRow App {..} =
    [ toField _appUuid
    , toField _appAppId
    , toField _appName
    , toField _appServerDomain
    , toField _appClientUrl
    , toField _appEnabled
    , toField _appCreatedAt
    , toField _appUpdatedAt
    , toField _appServerUrl
    ]

instance FromRow App where
  fromRow = do
    _appUuid <- field
    _appAppId <- field
    _appName <- field
    _appServerDomain <- field
    _appClientUrl <- field
    _appEnabled <- field
    _appCreatedAt <- field
    _appUpdatedAt <- field
    _appServerUrl <- field
    return $ App {..}
