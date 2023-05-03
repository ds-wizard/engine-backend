module Shared.Common.Model.Common.SensitiveData where

class SensitiveData a where
  process :: String -> a -> a
  process secret entity = entity
