module Wizard.Api.Resource.Submission.SubmissionServiceSimpleDTO where

import GHC.Generics

data SubmissionServiceSimpleDTO =
  SubmissionServiceSimpleDTO
    { _submissionServiceSimpleDTOId :: String
    , _submissionServiceSimpleDTOName :: String
    , _submissionServiceSimpleDTODescription :: String
    }
  deriving (Show, Eq, Generic)
