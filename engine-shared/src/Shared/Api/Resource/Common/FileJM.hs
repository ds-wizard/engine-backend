module Shared.Api.Resource.Common.FileJM where

import qualified Data.Text as T
import Servant.Multipart

import Shared.Api.Resource.Common.FileDTO

instance FromMultipart Mem FileDTO where
  fromMultipart form =
    FileDTO
      <$> fmap (T.unpack . fdFileName) (lookupFile "file" form)
      <*> fmap (T.unpack . fdFileCType) (lookupFile "file" form)
      <*> fmap fdPayload (lookupFile "file" form)
