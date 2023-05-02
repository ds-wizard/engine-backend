module Wizard.Api.Resource.TemporaryFile.TemporaryFileDTO where

import GHC.Generics

data TemporaryFileDTO = TemporaryFileDTO
  { url :: String
  , contentType :: String
  }
  deriving (Show, Eq, Generic)
