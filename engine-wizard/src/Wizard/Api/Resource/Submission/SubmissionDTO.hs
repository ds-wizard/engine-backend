module Wizard.Api.Resource.Submission.SubmissionDTO where

import GHC.Generics

data SubmissionDTO =
  SubmissionDTO
    { _submissionDTOLocation :: Maybe String
    }
  deriving (Show, Eq, Generic)
