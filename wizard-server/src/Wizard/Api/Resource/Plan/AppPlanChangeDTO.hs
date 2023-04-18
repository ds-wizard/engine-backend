module Wizard.Api.Resource.Plan.AppPlanChangeDTO where

import Data.Time
import GHC.Generics

data AppPlanChangeDTO = AppPlanChangeDTO
  { name :: String
  , users :: Maybe Int
  , since :: Maybe UTCTime
  , until :: Maybe UTCTime
  , test :: Bool
  }
  deriving (Generic)
