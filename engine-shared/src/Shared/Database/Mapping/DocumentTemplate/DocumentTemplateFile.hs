module Shared.Database.Mapping.DocumentTemplate.DocumentTemplateFile where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Model.DocumentTemplate.DocumentTemplate

instance ToRow DocumentTemplateFile where
  toRow DocumentTemplateFile {..} =
    [ toField documentTemplateId
    , toField uuid
    , toField fileName
    , toField content
    , toField appUuid
    , toField createdAt
    , toField updatedAt
    ]

instance FromRow DocumentTemplateFile where
  fromRow = do
    documentTemplateId <- field
    uuid <- field
    fileName <- field
    content <- field
    appUuid <- field
    createdAt <- field
    updatedAt <- field
    return $ DocumentTemplateFile {..}
