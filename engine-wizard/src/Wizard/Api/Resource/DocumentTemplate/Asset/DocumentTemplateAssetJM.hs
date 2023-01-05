module Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetDTO

instance FromJSON DocumentTemplateAssetDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateAssetDTO where
  toJSON = genericToJSON jsonOptions
