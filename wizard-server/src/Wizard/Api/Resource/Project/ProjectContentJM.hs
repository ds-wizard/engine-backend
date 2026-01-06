module Wizard.Api.Resource.Project.ProjectContentJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Project.Comment.ProjectCommentThreadListJM ()
import Wizard.Api.Resource.Project.Event.ProjectEventListJM ()
import Wizard.Api.Resource.Project.ProjectContentDTO
import Wizard.Api.Resource.Project.ProjectReplyJM ()
import Wizard.Api.Resource.Project.Version.ProjectVersionListJM ()

instance FromJSON ProjectContentDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectContentDTO where
  toJSON = genericToJSON jsonOptions
