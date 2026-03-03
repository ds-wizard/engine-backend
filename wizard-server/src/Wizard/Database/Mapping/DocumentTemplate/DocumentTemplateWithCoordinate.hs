module Wizard.Database.Mapping.DocumentTemplate.DocumentTemplateWithCoordinate where

import Database.PostgreSQL.Simple.FromRow

import Wizard.Model.DocumentTemplate.DocumentTemplateWithCoordinate

fieldDocumentTemplateWithCoordinate = do
  uuid <- field
  name <- field
  organizationId <- field
  templateId <- field
  version <- field
  return $ DocumentTemplateWithCoordinate {..}
