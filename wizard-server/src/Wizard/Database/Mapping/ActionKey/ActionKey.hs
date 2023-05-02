module Wizard.Database.Mapping.ActionKey.ActionKey where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Database.Mapping.ActionKey.ActionKeyType ()
import Wizard.Model.ActionKey.ActionKey

instance ToRow ActionKey where
  toRow ActionKey {..} =
    [ toField uuid
    , toField userId
    , toField aType
    , toField hash
    , toField createdAt
    , toField appUuid
    ]

instance FromRow ActionKey where
  fromRow = do
    uuid <- field
    userId <- field
    aType <- field
    hash <- field
    createdAt <- field
    appUuid <- field
    return $ ActionKey {..}
