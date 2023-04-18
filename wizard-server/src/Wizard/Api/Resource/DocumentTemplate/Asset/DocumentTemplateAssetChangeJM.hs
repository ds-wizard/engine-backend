module Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeDTO

instance FromJSON DocumentTemplateAssetChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateAssetChangeDTO where
  toJSON = genericToJSON jsonOptions
