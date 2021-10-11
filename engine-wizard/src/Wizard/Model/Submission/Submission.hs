module Wizard.Model.Submission.Submission where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data SubmissionState
  = InProgressSubmissionState
  | DoneSubmissionState
  | ErrorSubmissionState
  deriving (Show, Eq, Generic, Read)

data Submission =
  Submission
    { _submissionUuid :: U.UUID
    , _submissionState :: SubmissionState
    , _submissionLocation :: Maybe String
    , _submissionReturnedData :: Maybe String
    , _submissionServiceId :: String
    , _submissionDocumentUuid :: U.UUID
    , _submissionCreatedBy :: U.UUID
    , _submissionCreatedAt :: UTCTime
    , _submissionUpdatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
