module WizardLib.DocumentTemplate.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleDTO
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleJM ()
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import WizardLib.DocumentTemplate.Service.DocumentTemplate.Bundle.DocumentTemplateBundleMapper

instance ToSchema DocumentTemplateBundleDTO where
  declareNamedSchema = toSwagger (toBundle wizardDocumentTemplate [] [])
