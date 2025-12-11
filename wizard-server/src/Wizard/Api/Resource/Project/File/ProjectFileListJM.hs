module Wizard.Api.Resource.Project.File.ProjectFileListJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Project.ProjectSimpleJM ()
import Wizard.Model.Project.File.ProjectFileList
import WizardLib.Public.Api.Resource.User.UserSuggestionJM ()

instance FromJSON ProjectFileList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectFileList where
  toJSON = genericToJSON jsonOptions
