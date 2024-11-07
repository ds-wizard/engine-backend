module Wizard.Api.Resource.File.FileCreateJM where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import Servant.Multipart

import Wizard.Api.Resource.File.FileCreateDTO

instance FromMultipart Mem FileCreateDTO where
  fromMultipart form =
    FileCreateDTO
      <$> fmap T.unpack (lookupInput "fileName" form)
      <*> fmap (T.unpack . fdFileCType) (lookupFile "file" form)
      <*> fmap (BSL.toStrict . fdPayload) (lookupFile "file" form)
