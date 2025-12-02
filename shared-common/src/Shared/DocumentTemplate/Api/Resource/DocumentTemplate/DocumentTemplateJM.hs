module Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateFormatSimpleJM ()
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()

instance FromJSON DocumentTemplateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON DocumentTemplateFormatDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateFormatDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON DocumentTemplateFormatStepDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateFormatStepDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON DocumentTemplateFileDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateFileDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON DocumentTemplateAssetDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateAssetDTO where
  toJSON = genericToJSON jsonOptions
