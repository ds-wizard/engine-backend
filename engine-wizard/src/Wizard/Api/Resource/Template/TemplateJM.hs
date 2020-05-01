module Wizard.Api.Resource.Template.TemplateJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Template.TemplateDTO

instance FromJSON TemplateDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON TemplateDTO where
  toJSON = genericToJSON simpleOptions

instance FromJSON TemplateAllowedKMDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON TemplateAllowedKMDTO where
  toJSON = genericToJSON simpleOptions

instance FromJSON TemplateFormatDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON TemplateFormatDTO where
  toJSON = genericToJSON simpleOptions
