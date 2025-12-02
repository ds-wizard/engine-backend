module Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateFormatSimpleJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFormatSimple

instance FromJSON DocumentTemplateFormatSimple where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateFormatSimple where
  toJSON = genericToJSON jsonOptions
