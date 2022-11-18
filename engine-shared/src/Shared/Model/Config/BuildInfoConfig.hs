module Shared.Model.Config.BuildInfoConfig where

import GHC.Generics

data BuildInfoConfig = BuildInfoConfig
  { name :: String
  , version :: String
  , builtAt :: String
  }
  deriving (Generic, Show)
