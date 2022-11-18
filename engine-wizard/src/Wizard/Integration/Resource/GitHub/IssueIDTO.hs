module Wizard.Integration.Resource.GitHub.IssueIDTO where

import GHC.Generics

data IssueIDTO = IssueIDTO
  { number :: Int
  , title :: String
  , body :: String
  }
  deriving (Show, Eq, Generic)
