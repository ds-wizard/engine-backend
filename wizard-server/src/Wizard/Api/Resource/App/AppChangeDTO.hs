module Wizard.Api.Resource.App.AppChangeDTO where

import GHC.Generics

data AppChangeDTO = AppChangeDTO
  { appId :: String
  , name :: String
  }
  deriving (Generic)
