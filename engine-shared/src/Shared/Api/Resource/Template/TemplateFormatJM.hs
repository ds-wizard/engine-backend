module Shared.Api.Resource.Template.TemplateFormatJM where

import Data.Aeson

import Shared.Api.Resource.Template.TemplateFormatDTO
import Shared.Util.Aeson

instance FromJSON TemplateFormatDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TemplateFormatDTO where
  toJSON = genericToJSON jsonOptions
