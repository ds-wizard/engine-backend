module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeJM ()
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts

instance ToSchema DocumentTemplateDraftChangeDTO where
  declareNamedSchema = toSwagger wizardDocumentTemplateDraftChangeDTO
