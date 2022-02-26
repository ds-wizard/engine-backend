module Wizard.Api.Resource.App.AppCreateDTO where

import GHC.Generics

data AppCreateDTO =
  AppCreateDTO
    { _appCreateDTOAppId :: String
    , _appCreateDTOFirstName :: String
    , _appCreateDTOLastName :: String
    , _appCreateDTOEmail :: String
    , _appCreateDTOPassword :: String
    }
  deriving (Generic)
