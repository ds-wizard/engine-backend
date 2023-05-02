module Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetCreateDTO where

import qualified Data.ByteString.Char8 as BS

data DocumentTemplateAssetCreateDTO = DocumentTemplateAssetCreateDTO
  { fileName :: String
  , contentType :: String
  , content :: BS.ByteString
  }
