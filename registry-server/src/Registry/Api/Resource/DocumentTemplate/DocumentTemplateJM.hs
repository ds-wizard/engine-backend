module Registry.Api.Resource.DocumentTemplate.DocumentTemplateJM where

import Data.Aeson

import Registry.Api.Resource.DocumentTemplate.DocumentTemplateFormatJM ()
import Shared.Common.Util.Aeson
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateDTO

instance ToJSON DocumentTemplateFileDTO where
  toJSON = genericToJSON jsonOptions

instance ToJSON DocumentTemplateAssetDTO where
  toJSON = genericToJSON jsonOptions
