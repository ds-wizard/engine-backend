module Shared.Api.Resource.DocumentTemplate.DocumentTemplateFormatJM where

import Data.Aeson

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateFormatDTO
import Shared.Util.Aeson

instance FromJSON DocumentTemplateFormatDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateFormatDTO where
  toJSON = genericToJSON jsonOptions
