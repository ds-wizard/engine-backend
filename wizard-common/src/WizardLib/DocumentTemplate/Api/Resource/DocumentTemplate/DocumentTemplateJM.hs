module WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateFormatJM ()
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()

instance FromJSON DocumentTemplateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON DocumentTemplateFileDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateFileDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON DocumentTemplateAssetDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateAssetDTO where
  toJSON = genericToJSON jsonOptions
