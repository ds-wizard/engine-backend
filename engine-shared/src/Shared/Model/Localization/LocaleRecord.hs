module Shared.Model.Localization.LocaleRecord where

type LocaleKey = String

type DefaultValue = String

type Variable = String

data LocaleRecord =
  LocaleRecord LocaleKey DefaultValue [Variable]
  deriving (Show, Eq)
