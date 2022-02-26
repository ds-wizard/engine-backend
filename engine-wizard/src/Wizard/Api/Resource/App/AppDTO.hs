module Wizard.Api.Resource.App.AppDTO where

import GHC.Generics

data AppDTO =
  AppDTO
    { _appDTOClientUrl :: String
    }
  deriving (Show, Eq, Generic)
