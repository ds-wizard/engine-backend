module Wizard.Api.Resource.Locale.LocaleDTO where

import GHC.Generics

data LocaleDTO = LocaleDTO
  { name :: String
  , code :: String
  , fallback :: Bool
  }
  deriving (Show, Eq, Generic)
