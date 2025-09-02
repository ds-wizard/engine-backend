module WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateFormatSimpleJM ()
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionDTO

instance FromJSON DocumentTemplateSuggestionDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateSuggestionDTO where
  toJSON = genericToJSON jsonOptions
