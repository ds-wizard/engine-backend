module Wizard.Api.Resource.Config.SimpleFeatureDTO where

import GHC.Generics

data SimpleFeatureDTO =
  SimpleFeatureDTO
    { _simpleFeatureDTOEnabled :: Bool
    }
  deriving (Show, Eq, Generic)
