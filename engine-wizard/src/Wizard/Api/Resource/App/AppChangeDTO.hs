module Wizard.Api.Resource.App.AppChangeDTO where

import GHC.Generics

data AppChangeDTO =
  AppChangeDTO
    { _appChangeDTOAppId :: String
    , _appChangeDTOName :: String
    }
  deriving (Generic)
