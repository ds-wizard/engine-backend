module Wizard.Api.Resource.Branch.BranchWithEventsJM where

import Data.Aeson

import Shared.Api.Resource.Event.EventJM ()
import Shared.Util.JSON
import Wizard.Api.Resource.Branch.BranchWithEventsDTO

instance FromJSON BranchWithEventsDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON BranchWithEventsDTO where
  toJSON = genericToJSON simpleOptions
