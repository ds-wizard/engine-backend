module Wizard.Api.Resource.Submission.SubmissionServiceSimpleDTO where

import GHC.Generics

data SubmissionServiceSimpleDTO = SubmissionServiceSimpleDTO
  { sId :: String
  , name :: String
  , description :: String
  }
  deriving (Show, Eq, Generic)
