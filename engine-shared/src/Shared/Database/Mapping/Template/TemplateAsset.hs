module Shared.Database.Mapping.Template.TemplateAsset where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Model.Template.Template

instance ToRow TemplateAsset where
  toRow TemplateAsset {..} =
    [ toField templateId
    , toField uuid
    , toField fileName
    , toField contentType
    , toField appUuid
    , toField fileSize
    ]

instance FromRow TemplateAsset where
  fromRow = do
    templateId <- field
    uuid <- field
    fileName <- field
    contentType <- field
    appUuid <- field
    fileSize <- field
    return $ TemplateAsset {..}
