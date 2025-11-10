module Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateJM ()
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts

instance ToSchema DocumentTemplateDraftCreateDTO where
  declareNamedSchema = toSwagger wizardDocumentTemplateDraftCreateDTO
