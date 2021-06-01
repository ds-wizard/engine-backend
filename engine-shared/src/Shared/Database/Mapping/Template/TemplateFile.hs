module Shared.Database.Mapping.Template.TemplateFile where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Model.Template.Template

instance ToRow TemplateFile where
  toRow TemplateFile {..} =
    [ toField _templateFileTemplateId
    , toField _templateFileUuid
    , toField _templateFileFileName
    , toField _templateFileContent
    ]

instance FromRow TemplateFile where
  fromRow = do
    _templateFileTemplateId <- field
    _templateFileUuid <- field
    _templateFileFileName <- field
    _templateFileContent <- field
    return $ TemplateFile {..}
