module Shared.Model.Config.BuildInfoConfig where

import Data.Time
import GHC.Generics

data BuildInfoConfig = BuildInfoConfig
  { name :: String
  , version :: String
  , releaseVersion :: String
  , builtAt :: UTCTime
  }
  deriving (Generic, Show)
