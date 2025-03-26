module Shared.Locale.Model.Locale.LocaleSuggestion where

import GHC.Generics

data LocaleSuggestion = LocaleSuggestion
  { lId :: String
  , name :: String
  , description :: String
  , code :: String
  , defaultLocale :: Bool
  }
  deriving (Show, Eq, Generic)
