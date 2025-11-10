module Shared.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplateFile where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

instance ToRow DocumentTemplateFile where
  toRow DocumentTemplateFile {..} =
    [ toField documentTemplateId
    , toField uuid
    , toField fileName
    , toField content
    , toField tenantUuid
    , toField createdAt
    , toField updatedAt
    ]

instance FromRow DocumentTemplateFile where
  fromRow = do
    documentTemplateId <- field
    uuid <- field
    fileName <- field
    content <- field
    tenantUuid <- field
    createdAt <- field
    updatedAt <- field
    return $ DocumentTemplateFile {..}
