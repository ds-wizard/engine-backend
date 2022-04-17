module Wizard.Api.Resource.Plan.AppPlanChangeDTO where

import Data.Time
import GHC.Generics

data AppPlanChangeDTO =
  AppPlanChangeDTO
    { _appPlanChangeDTOName :: String
    , _appPlanChangeDTOUsers :: Maybe Int
    , _appPlanChangeDTOSince :: Maybe UTCTime
    , _appPlanChangeDTOUntil :: Maybe UTCTime
    , _appPlanChangeDTOTest :: Bool
    }
  deriving (Generic)
