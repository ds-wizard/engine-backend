module Wizard.Model.Locale.LocaleState where

import GHC.Generics

data LocaleState
  = UnknownLocaleState
  | OutdatedLocaleState
  | UpToDateLocaleState
  | UnpublishedLocaleState
  deriving (Show, Eq, Generic, Read)
