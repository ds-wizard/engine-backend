module Shared.Database.Mapping.DocumentTemplate.DocumentTemplateAsset where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Model.DocumentTemplate.DocumentTemplate

instance ToRow DocumentTemplateAsset where
  toRow DocumentTemplateAsset {..} =
    [ toField documentTemplateId
    , toField uuid
    , toField fileName
    , toField contentType
    , toField appUuid
    , toField fileSize
    , toField createdAt
    , toField updatedAt
    ]

instance FromRow DocumentTemplateAsset where
  fromRow = do
    documentTemplateId <- field
    uuid <- field
    fileName <- field
    contentType <- field
    appUuid <- field
    fileSize <- field
    createdAt <- field
    updatedAt <- field
    return $ DocumentTemplateAsset {..}
