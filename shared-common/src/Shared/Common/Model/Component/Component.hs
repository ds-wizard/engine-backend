module Shared.Common.Model.Component.Component where

import Data.Time
import GHC.Generics

data Component = Component
  { name :: String
  , version :: String
  , builtAt :: UTCTime
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Eq, Show)
