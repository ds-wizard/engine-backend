module Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSimpleJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateSimple

instance FromJSON DocumentTemplateSimple where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateSimple where
  toJSON = genericToJSON jsonOptions
