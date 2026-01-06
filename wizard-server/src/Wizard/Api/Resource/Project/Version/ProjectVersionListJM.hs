module Wizard.Api.Resource.Project.Version.ProjectVersionListJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.Project.Version.ProjectVersion
import Wizard.Model.Project.Version.ProjectVersionList
import WizardLib.Public.Api.Resource.User.UserSuggestionJM ()

instance FromJSON ProjectVersion where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectVersion where
  toJSON = genericToJSON jsonOptions

instance FromJSON ProjectVersionList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectVersionList where
  toJSON = genericToJSON jsonOptions
