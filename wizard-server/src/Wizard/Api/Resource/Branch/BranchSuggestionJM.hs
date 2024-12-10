module Wizard.Api.Resource.Branch.BranchSuggestionJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.Branch.BranchSuggestion

instance FromJSON BranchSuggestion where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON BranchSuggestion where
  toJSON = genericToJSON jsonOptions
