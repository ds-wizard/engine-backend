module Wizard.Integration.Resource.GitHub.IssueCreateIJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Integration.Resource.GitHub.IssueCreateIDTO

instance FromJSON IssueCreateIDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON IssueCreateIDTO where
  toJSON = genericToJSON jsonOptions
