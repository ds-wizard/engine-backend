module Shared.Api.Resource.DocumentTemplate.DocumentTemplateJM where

import Data.Aeson

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateFormatJM ()
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Util.Aeson

instance FromJSON DocumentTemplateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON DocumentTemplateFileDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateFileDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON DocumentTemplateAssetDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateAssetDTO where
  toJSON = genericToJSON jsonOptions
