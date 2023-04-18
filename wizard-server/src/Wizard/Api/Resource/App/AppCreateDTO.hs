module Wizard.Api.Resource.App.AppCreateDTO where

import GHC.Generics

data AppCreateDTO = AppCreateDTO
  { appId :: String
  , appName :: String
  , firstName :: String
  , lastName :: String
  , email :: String
  , password :: String
  }
  deriving (Generic)
