module Wizard.Api.Resource.Project.Comment.ProjectCommentThreadListJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.Project.Comment.ProjectCommentList
import WizardLib.Public.Api.Resource.User.UserSuggestionJM ()

instance FromJSON ProjectCommentThreadList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectCommentThreadList where
  toJSON = genericToJSON jsonOptions

instance FromJSON ProjectCommentList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectCommentList where
  toJSON = genericToJSON jsonOptions
