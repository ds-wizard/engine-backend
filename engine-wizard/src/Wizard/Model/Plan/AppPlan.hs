module Wizard.Model.Plan.AppPlan where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data AppPlan = AppPlan
  { uuid :: U.UUID
  , name :: String
  , users :: Maybe Int
  , since :: Maybe UTCTime
  , until :: Maybe UTCTime
  , test :: Bool
  , appUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
