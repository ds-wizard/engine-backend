module Shared.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleJM where

import Data.Aeson

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleDTO
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Util.Aeson

instance FromJSON DocumentTemplateBundleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateBundleDTO where
  toJSON = genericToJSON jsonOptions
