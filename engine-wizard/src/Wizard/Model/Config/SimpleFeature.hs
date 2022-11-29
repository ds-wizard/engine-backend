module Wizard.Model.Config.SimpleFeature where

import GHC.Generics

data SimpleFeature = SimpleFeature
  { enabled :: Bool
  }
  deriving (Show, Eq, Generic)
