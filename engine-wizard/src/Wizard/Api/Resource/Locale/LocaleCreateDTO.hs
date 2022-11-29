module Wizard.Api.Resource.Locale.LocaleCreateDTO where

import GHC.Generics

data LocaleCreateDTO = LocaleCreateDTO
  { name :: String
  , description :: String
  , code :: String
  , localeId :: String
  , version :: String
  , license :: String
  , readme :: String
  , recommendedAppVersion :: String
  }
  deriving (Show, Eq, Generic)
