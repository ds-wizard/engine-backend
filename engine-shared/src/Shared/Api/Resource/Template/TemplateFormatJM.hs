module Shared.Api.Resource.Template.TemplateFormatJM where

import Data.Aeson

import Shared.Api.Resource.Template.TemplateFormatDTO
import Shared.Util.JSON

instance FromJSON TemplateFormatDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON TemplateFormatDTO where
  toJSON = genericToJSON simpleOptions
