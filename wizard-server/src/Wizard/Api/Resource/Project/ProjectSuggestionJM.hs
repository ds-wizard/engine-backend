module Wizard.Api.Resource.Project.ProjectSuggestionJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.Project.ProjectSuggestion

instance FromJSON ProjectSuggestion where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectSuggestion where
  toJSON = genericToJSON jsonOptions
