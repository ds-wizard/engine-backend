module Shared.Api.Resource.Template.TemplateJM where

import Data.Aeson

import Shared.Api.Resource.Template.TemplateDTO
import Shared.Api.Resource.Template.TemplateFormatJM ()
import Shared.Util.Aeson

instance FromJSON TemplateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TemplateDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON TemplateFileDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TemplateFileDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON TemplateAssetDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TemplateAssetDTO where
  toJSON = genericToJSON jsonOptions
