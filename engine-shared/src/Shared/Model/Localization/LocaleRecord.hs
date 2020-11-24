module Shared.Model.Localization.LocaleRecord where

import GHC.Generics

data LocaleRecord =
  LocaleRecord
    { _localeRecordCode :: String
    , _localeRecordDefaultMessage :: String
    , _localeRecordParams :: [String]
    }
  deriving (Show, Eq, Generic)
