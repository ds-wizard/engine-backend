module Shared.Locale.Model.Locale.LocaleSuggestion where

import qualified Data.UUID as U
import GHC.Generics

data LocaleSuggestion = LocaleSuggestion
  { uuid :: U.UUID
  , name :: String
  , description :: String
  , code :: String
  , organizationId :: String
  , localeId :: String
  , version :: String
  , defaultLocale :: Bool
  }
  deriving (Show, Eq, Generic)
