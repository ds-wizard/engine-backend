module Wizard.Api.Resource.Submission.SubmissionDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.User.UserSuggestionDTO
import Wizard.Model.Submission.Submission

data SubmissionDTO =
  SubmissionDTO
    { _submissionDTOUuid :: U.UUID
    , _submissionDTOState :: SubmissionState
    , _submissionDTOLocation :: Maybe String
    , _submissionDTOReturnedData :: Maybe String
    , _submissionDTOServiceId :: String
    , _submissionDTOServiceName :: Maybe String
    , _submissionDTODocumentUuid :: U.UUID
    , _submissionDTOCreatedBy :: UserSuggestionDTO
    , _submissionDTOCreatedAt :: UTCTime
    , _submissionDTOUpdatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
