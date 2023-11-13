module Wizard.Model.Feedback.Feedback where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data Feedback = Feedback
  { uuid :: U.UUID
  , issueId :: Int
  , questionUuid :: U.UUID
  , packageId :: String
  , title :: String
  , content :: String
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq Feedback where
  a == b =
    uuid a == uuid b
      && issueId a == issueId b
      && questionUuid a == questionUuid b
      && packageId a == packageId b
      && title a == title b
      && content a == content b
      && tenantUuid a == tenantUuid b
