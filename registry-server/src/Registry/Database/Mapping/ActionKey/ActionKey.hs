module Registry.Database.Mapping.ActionKey.ActionKey where

import Database.PostgreSQL.Simple

import Registry.Database.Mapping.ActionKey.ActionKeyType ()
import Registry.Model.ActionKey.ActionKey

instance ToRow ActionKey

instance FromRow ActionKey
