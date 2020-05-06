module Wizard.Api.Resource.Submission.SubmissionCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

data SubmissionCreateDTO =
  SubmissionCreateDTO
    { _submissionCreateDTOServiceId :: String
    , _submissionCreateDTODocUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)
