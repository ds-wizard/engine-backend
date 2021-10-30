module Wizard.Database.Mapping.ActionKey.ActionKey where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Database.Mapping.ActionKey.ActionKeyType ()
import Wizard.Model.ActionKey.ActionKey

instance ToRow ActionKey where
  toRow ActionKey {..} =
    [ toField _actionKeyUuid
    , toField _actionKeyUserId
    , toField _actionKeyAType
    , toField _actionKeyHash
    , toField _actionKeyCreatedAt
    , toField _actionKeyAppUuid
    ]

instance FromRow ActionKey where
  fromRow = do
    _actionKeyUuid <- field
    _actionKeyUserId <- field
    _actionKeyAType <- field
    _actionKeyHash <- field
    _actionKeyCreatedAt <- field
    _actionKeyAppUuid <- field
    return $ ActionKey {..}
