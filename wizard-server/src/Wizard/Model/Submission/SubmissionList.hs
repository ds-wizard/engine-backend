module Wizard.Model.Submission.SubmissionList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Submission.Submission
import WizardLib.Public.Api.Resource.User.UserSuggestionDTO

data SubmissionList = SubmissionList
  { uuid :: U.UUID
  , state :: SubmissionState
  , location :: Maybe String
  , returnedData :: Maybe String
  , serviceId :: String
  , serviceName :: Maybe String
  , documentUuid :: U.UUID
  , createdBy :: UserSuggestionDTO
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
