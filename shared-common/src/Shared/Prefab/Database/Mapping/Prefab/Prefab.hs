module Shared.Prefab.Database.Mapping.Prefab.Prefab where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Prefab.Model.Prefab.Prefab

instance ToRow Prefab where
  toRow Prefab {..} =
    [ toField uuid
    , toField pType
    , toField name
    , toJSONField content
    , toField appUuid
    , toField createdAt
    , toField updatedAt
    ]

instance FromRow Prefab where
  fromRow = do
    uuid <- field
    pType <- field
    name <- field
    content <- fieldWith fromJSONField
    appUuid <- field
    createdAt <- field
    updatedAt <- field
    return $ Prefab {..}
