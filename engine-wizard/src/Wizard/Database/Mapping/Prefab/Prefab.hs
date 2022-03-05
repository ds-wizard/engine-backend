module Wizard.Database.Mapping.Prefab.Prefab where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Model.Prefab.Prefab

instance ToRow Prefab where
  toRow Prefab {..} =
    [ toField _prefabUuid
    , toField _prefabPType
    , toField _prefabName
    , toJSONField _prefabContent
    , toField _prefabAppUuid
    , toField _prefabCreatedAt
    , toField _prefabUpdatedAt
    ]

instance FromRow Prefab where
  fromRow = do
    _prefabUuid <- field
    _prefabPType <- field
    _prefabName <- field
    _prefabContent <- fieldWith fromJSONField
    _prefabAppUuid <- field
    _prefabCreatedAt <- field
    _prefabUpdatedAt <- field
    return $ Prefab {..}
