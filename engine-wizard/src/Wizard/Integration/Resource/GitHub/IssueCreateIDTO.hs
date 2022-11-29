module Wizard.Integration.Resource.GitHub.IssueCreateIDTO where

import GHC.Generics

data IssueCreateIDTO = IssueCreateIDTO
  { title :: String
  , body :: String
  , assignees :: [String]
  , milestone :: Maybe String
  , labels :: [String]
  }
  deriving (Show, Eq, Generic)
