module Shared.Api.Resource.TemplateBundle.TemplateBundleJM where

import Data.Aeson

import Shared.Api.Resource.Template.TemplateJM ()
import Shared.Api.Resource.TemplateBundle.TemplateBundleDTO
import Shared.Model.Template.TemplateJM ()
import Shared.Util.JSON

instance FromJSON TemplateBundleDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON TemplateBundleDTO where
  toJSON = genericToJSON simpleOptions
