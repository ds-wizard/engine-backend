module Wizard.Api.Resource.Config.AppConfigChangeDTO where

import GHC.Generics

import Wizard.Api.Resource.Config.AppConfigDTO

data AppConfigChangeDTO =
  AppConfigChangeDTO
    { _appConfigChangeDTOFeatures :: AppConfigFeaturesDTO
    , _appConfigChangeDTOClient :: AppConfigClientDTO
    }
  deriving (Generic, Eq, Show)
