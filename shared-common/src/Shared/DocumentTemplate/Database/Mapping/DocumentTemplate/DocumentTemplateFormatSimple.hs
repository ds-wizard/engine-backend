module Shared.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplateFormatSimple where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFormatSimple

instance FromRow DocumentTemplateFormatSimple

fieldDocumentTemplateFormatSimple :: RowParser DocumentTemplateFormatSimple
fieldDocumentTemplateFormatSimple = do
  uuid <- field
  name <- field
  icon <- field
  return DocumentTemplateFormatSimple {..}
