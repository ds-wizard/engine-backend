module Wizard.Api.Resource.Branch.BranchCreateJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.Branch.BranchCreateDTO

instance FromJSON BranchCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON BranchCreateDTO where
  toJSON = genericToJSON jsonOptions
