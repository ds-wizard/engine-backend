module Wizard.Api.Resource.Branch.BranchCreateJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Branch.BranchCreateDTO

instance FromJSON BranchCreateDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON BranchCreateDTO where
  toJSON = genericToJSON simpleOptions
