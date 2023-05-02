module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftListSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftListJM ()
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftList
import Wizard.Service.DocumentTemplate.Draft.DocumentTemplateDraftMapper
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates

instance ToSchema DocumentTemplateDraftList where
  declareNamedSchema = toSwagger (toDraftList wizardDocumentTemplate)
