module Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeDTO where

import GHC.Generics

data DocumentTemplateAssetChangeDTO = DocumentTemplateAssetChangeDTO
  { fileName :: String
  }
  deriving (Show, Eq, Generic)
