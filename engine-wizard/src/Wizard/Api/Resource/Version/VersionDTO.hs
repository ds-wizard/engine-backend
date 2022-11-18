module Wizard.Api.Resource.Version.VersionDTO where

import GHC.Generics

data VersionDTO = VersionDTO
  { description :: String
  , readme :: String
  , license :: String
  }
  deriving (Generic)
