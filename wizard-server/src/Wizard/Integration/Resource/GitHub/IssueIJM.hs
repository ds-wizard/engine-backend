module Wizard.Integration.Resource.GitHub.IssueIJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Integration.Resource.GitHub.IssueIDTO

instance FromJSON IssueIDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON IssueIDTO where
  toJSON = genericToJSON jsonOptions
