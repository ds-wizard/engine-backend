module Wizard.Integration.Resource.GitHub.IssueIDTO where

import GHC.Generics

data IssueIDTO =
  IssueIDTO
    { _issueIDTOId :: Int
    , _issueIDTOTitle :: String
    , _issueIDTOBody :: String
    }
  deriving (Show, Eq, Generic)
