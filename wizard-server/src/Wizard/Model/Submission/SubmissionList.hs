module Wizard.Model.Submission.SubmissionList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Submission.Submission
import WizardLib.Public.Model.User.UserSuggestion

data SubmissionList = SubmissionList
  { uuid :: U.UUID
  , state :: SubmissionState
  , location :: Maybe String
  , returnedData :: Maybe String
  , serviceId :: String
  , serviceName :: Maybe String
  , documentUuid :: U.UUID
  , createdBy :: UserSuggestion
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
