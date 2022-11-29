module Shared.Database.Mapping.Template.TemplateFile where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Model.Template.Template

instance ToRow TemplateFile where
  toRow TemplateFile {..} =
    [ toField templateId
    , toField uuid
    , toField fileName
    , toField content
    , toField appUuid
    ]

instance FromRow TemplateFile where
  fromRow = do
    templateId <- field
    uuid <- field
    fileName <- field
    content <- field
    appUuid <- field
    return $ TemplateFile {..}
