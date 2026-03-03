module Wizard.Api.Resource.DocumentTemplate.DocumentTemplateWithCoordinateSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateWithCoordinateJM ()
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Wizard.Model.DocumentTemplate.DocumentTemplateWithCoordinate

instance ToSchema DocumentTemplateWithCoordinate where
  declareNamedSchema = toSwagger wizardDocumentTemplateWithCoordinate
