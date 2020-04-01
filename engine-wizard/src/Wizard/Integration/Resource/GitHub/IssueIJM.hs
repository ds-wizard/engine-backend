module Wizard.Integration.Resource.GitHub.IssueIJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Integration.Resource.GitHub.IssueIDTO

instance FromJSON IssueIDTO where
  parseJSON = simpleParseJSON "_issueIDTO"

instance ToJSON IssueIDTO where
  toJSON = simpleToJSON "_issueIDTO"
