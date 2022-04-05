module Wizard.Model.Plan.AppPlan where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data AppPlan =
  AppPlan
    { _appPlanUuid :: U.UUID
    , _appPlanName :: String
    , _appPlanUsers :: Maybe Int
    , _appPlanSince :: UTCTime
    , _appPlanUntil :: UTCTime
    , _appPlanTest :: Bool
    , _appPlanAppUuid :: U.UUID
    , _appPlanCreatedAt :: UTCTime
    , _appPlanUpdatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
