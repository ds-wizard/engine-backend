module Wizard.Api.Resource.File.FileCreateDTO where

import qualified Data.ByteString.Char8 as BS

data FileCreateDTO = FileCreateDTO
  { fileName :: String
  , contentType :: String
  , content :: BS.ByteString
  }
