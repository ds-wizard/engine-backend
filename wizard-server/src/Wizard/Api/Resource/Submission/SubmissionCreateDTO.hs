module Wizard.Api.Resource.Submission.SubmissionCreateDTO where

import GHC.Generics

data SubmissionCreateDTO = SubmissionCreateDTO
  { serviceId :: String
  }
  deriving (Show, Eq, Generic)
