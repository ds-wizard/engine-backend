module Shared.Api.Resource.TemplateBundle.TemplateBundleJM where

import Data.Aeson

import Shared.Api.Resource.Template.TemplateJM ()
import Shared.Api.Resource.TemplateBundle.TemplateBundleDTO
import Shared.Model.Template.TemplateJM ()
import Shared.Util.Aeson

instance FromJSON TemplateBundleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TemplateBundleDTO where
  toJSON = genericToJSON jsonOptions
