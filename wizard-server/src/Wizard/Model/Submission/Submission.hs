module Wizard.Model.Submission.Submission where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data SubmissionState
  = InProgressSubmissionState
  | DoneSubmissionState
  | ErrorSubmissionState
  deriving (Show, Eq, Generic, Read)

data Submission = Submission
  { uuid :: U.UUID
  , state :: SubmissionState
  , location :: Maybe String
  , returnedData :: Maybe String
  , serviceId :: String
  , documentUuid :: U.UUID
  , appUuid :: U.UUID
  , createdBy :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
