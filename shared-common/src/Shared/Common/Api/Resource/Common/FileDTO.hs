module Shared.Common.Api.Resource.Common.FileDTO where

import qualified Data.ByteString.Lazy.Char8 as BSL

data FileDTO = FileDTO
  { fileName :: String
  , contentType :: String
  , content :: BSL.ByteString
  }
