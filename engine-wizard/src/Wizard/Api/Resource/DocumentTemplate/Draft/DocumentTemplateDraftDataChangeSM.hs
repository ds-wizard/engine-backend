module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataChangeSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataChangeDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataChangeJM ()
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts

instance ToSchema DocumentTemplateDraftDataChangeDTO where
  declareNamedSchema = toSwagger wizardDocumentTemplateDraftDataChangeDTO
