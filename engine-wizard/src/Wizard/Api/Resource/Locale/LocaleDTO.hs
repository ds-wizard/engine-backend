module Wizard.Api.Resource.Locale.LocaleDTO where

import GHC.Generics

data LocaleDTO =
  LocaleDTO
    { _localeDTOName :: String
    , _localeDTOCode :: String
    , _localeDTOFallback :: Bool
    }
  deriving (Show, Eq, Generic)
