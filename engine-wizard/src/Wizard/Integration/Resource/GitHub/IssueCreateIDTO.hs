module Wizard.Integration.Resource.GitHub.IssueCreateIDTO where

import GHC.Generics

data IssueCreateIDTO =
  IssueCreateIDTO
    { _issueCreateIDTOTitle :: String
    , _issueCreateIDTOBody :: String
    , _issueCreateIDTOAssignees :: [String]
    , _issueCreateIDTOMilestone :: Maybe String
    , _issueCreateIDTOLabels :: [String]
    }
  deriving (Show, Eq, Generic)
