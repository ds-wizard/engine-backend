module Shared.Common.Model.Localization.LocaleRecord where

import GHC.Generics

data LocaleRecord = LocaleRecord
  { code :: String
  , defaultMessage :: String
  , params :: [String]
  }
  deriving (Show, Eq, Generic)
