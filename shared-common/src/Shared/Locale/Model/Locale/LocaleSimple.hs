module Shared.Locale.Model.Locale.LocaleSimple where

import qualified Data.UUID as U
import GHC.Generics

data LocaleSimple = LocaleSimple
  { uuid :: U.UUID
  , name :: String
  , code :: String
  , defaultLocale :: Bool
  }
  deriving (Show, Eq, Generic)
