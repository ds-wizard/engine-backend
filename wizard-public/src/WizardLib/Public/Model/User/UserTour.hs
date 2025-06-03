module WizardLib.Public.Model.User.UserTour where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data UserTour = UserTour
  { userUuid :: U.UUID
  , tourId :: String
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Generic, Eq, Show)
