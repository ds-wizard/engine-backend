module Wizard.Model.Locale.LocaleSimple where

import GHC.Generics

data LocaleSimple = LocaleSimple
  { lId :: String
  , name :: String
  , code :: String
  , defaultLocale :: Bool
  }
  deriving (Show, Eq, Generic)
