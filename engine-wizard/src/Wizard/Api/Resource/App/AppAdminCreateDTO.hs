module Wizard.Api.Resource.App.AppAdminCreateDTO where

import GHC.Generics

data AppAdminCreateDTO =
  AppAdminCreateDTO
    { _appAdminCreateDTOAppId :: String
    , _appAdminCreateDTOName :: String
    , _appAdminCreateDTOServerDomain :: String
    , _appAdminCreateDTOServerUrl :: String
    , _appAdminCreateDTOClientUrl :: String
    }
  deriving (Generic)
