module Wizard.Api.Resource.Branch.BranchChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Branch.BranchChangeDTO

instance FromJSON BranchChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON BranchChangeDTO where
  toJSON = genericToJSON jsonOptions
