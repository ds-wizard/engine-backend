module Wizard.Model.Config.SimpleFeature where

import GHC.Generics

data SimpleFeature =
  SimpleFeature
    { _simpleFeatureEnabled :: Bool
    }
  deriving (Show, Eq, Generic)
