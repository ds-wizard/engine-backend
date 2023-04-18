module WizardLib.DocumentTemplate.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleDTO
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()

instance FromJSON DocumentTemplateBundleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateBundleDTO where
  toJSON = genericToJSON jsonOptions
