module Wizard.Integration.Resource.GitHub.IssueCreateIJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Integration.Resource.GitHub.IssueCreateIDTO

instance FromJSON IssueCreateIDTO where
  parseJSON = simpleParseJSON "_issueCreateIDTO"

instance ToJSON IssueCreateIDTO where
  toJSON = simpleToJSON "_issueCreateIDTO"
