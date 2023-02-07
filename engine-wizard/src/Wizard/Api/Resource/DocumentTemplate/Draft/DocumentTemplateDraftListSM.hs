module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftListSM where

import Data.Swagger

import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Util.Swagger
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftListJM ()
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftList
import Wizard.Service.DocumentTemplate.Draft.DocumentTemplateDraftMapper

instance ToSchema DocumentTemplateDraftList where
  declareNamedSchema = toSwagger (toDraftList wizardDocumentTemplate)
