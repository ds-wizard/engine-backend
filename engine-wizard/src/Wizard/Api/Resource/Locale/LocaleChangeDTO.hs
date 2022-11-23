module Wizard.Api.Resource.Locale.LocaleChangeDTO where

import GHC.Generics

data LocaleChangeDTO = LocaleChangeDTO
  { defaultLocale :: Bool
  , enabled :: Bool
  }
  deriving (Show, Eq, Generic)
