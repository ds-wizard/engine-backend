module Wizard.Api.Resource.App.AppCreateDTO where

import GHC.Generics

data AppCreateDTO =
  AppCreateDTO
    { _appCreateDTOAppId :: String
    , _appCreateDTOName :: String
    , _appCreateDTOServerDomain :: String
    , _appCreateDTOServerUrl :: String
    , _appCreateDTOClientUrl :: String
    }
  deriving (Generic)
