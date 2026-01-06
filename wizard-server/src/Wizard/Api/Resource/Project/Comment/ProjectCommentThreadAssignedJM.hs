module Wizard.Api.Resource.Project.Comment.ProjectCommentThreadAssignedJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.Project.Comment.ProjectCommentThreadAssigned
import WizardLib.Public.Api.Resource.User.UserSuggestionJM ()

instance FromJSON ProjectCommentThreadAssigned where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectCommentThreadAssigned where
  toJSON = genericToJSON jsonOptions
