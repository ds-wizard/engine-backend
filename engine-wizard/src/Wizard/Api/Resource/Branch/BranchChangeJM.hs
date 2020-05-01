module Wizard.Api.Resource.Branch.BranchChangeJM where

import Data.Aeson

import Shared.Api.Resource.Event.EventJM ()
import Shared.Util.JSON
import Wizard.Api.Resource.Branch.BranchChangeDTO

instance FromJSON BranchChangeDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON BranchChangeDTO where
  toJSON = genericToJSON simpleOptions
