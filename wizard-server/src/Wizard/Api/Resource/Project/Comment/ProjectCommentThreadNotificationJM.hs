module Wizard.Api.Resource.Project.Comment.ProjectCommentThreadNotificationJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.User.UserSimpleJM ()
import Wizard.Model.Project.Comment.ProjectCommentThreadNotification

instance FromJSON ProjectCommentThreadNotification where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectCommentThreadNotification where
  toJSON = genericToJSON jsonOptions
