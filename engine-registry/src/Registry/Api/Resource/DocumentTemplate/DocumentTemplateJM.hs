module Registry.Api.Resource.DocumentTemplate.DocumentTemplateJM where

import Data.Aeson

import Registry.Api.Resource.DocumentTemplate.DocumentTemplateFormatJM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import Shared.Util.Aeson

instance ToJSON DocumentTemplateFileDTO where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentTemplateAssetDTO where
  toJSON = genericToJSON jsonOptions
