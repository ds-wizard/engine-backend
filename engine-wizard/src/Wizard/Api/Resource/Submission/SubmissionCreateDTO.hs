module Wizard.Api.Resource.Submission.SubmissionCreateDTO where

import GHC.Generics

data SubmissionCreateDTO =
  SubmissionCreateDTO
    { _submissionCreateDTOServiceId :: String
    }
  deriving (Show, Eq, Generic)
