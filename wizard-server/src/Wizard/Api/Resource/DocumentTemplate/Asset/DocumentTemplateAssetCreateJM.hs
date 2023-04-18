module Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetCreateJM where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import Servant.Multipart

import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetCreateDTO

instance FromMultipart Mem DocumentTemplateAssetCreateDTO where
  fromMultipart form =
    DocumentTemplateAssetCreateDTO
      <$> fmap T.unpack (lookupInput "fileName" form)
      <*> fmap (T.unpack . fdFileCType) (lookupFile "file" form)
      <*> fmap (BSL.toStrict . fdPayload) (lookupFile "file" form)
