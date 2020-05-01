module Wizard.Api.Resource.Branch.BranchJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Branch.BranchDTO
import Wizard.Api.Resource.Branch.BranchStateJM ()

instance FromJSON BranchDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON BranchDTO where
  toJSON = genericToJSON simpleOptions
