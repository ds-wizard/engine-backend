module Wizard.Database.Mapping.User.UserPluginSettings where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Model.User.UserPluginSettings

instance ToRow UserPluginSettings where
  toRow UserPluginSettings {..} =
    [ toField userUuid
    , toField pluginUuid
    , toJSONField values
    , toField tenantUuid
    , toField createdAt
    , toField updatedAt
    ]

instance FromRow UserPluginSettings where
  fromRow = do
    userUuid <- field
    pluginUuid <- field
    values <- fieldWith fromJSONField
    tenantUuid <- field
    createdAt <- field
    updatedAt <- field
    return $ UserPluginSettings {..}
