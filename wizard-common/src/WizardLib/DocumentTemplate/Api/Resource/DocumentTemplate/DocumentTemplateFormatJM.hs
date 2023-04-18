module WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateFormatJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateFormatDTO

instance FromJSON DocumentTemplateFormatDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateFormatDTO where
  toJSON = genericToJSON jsonOptions
