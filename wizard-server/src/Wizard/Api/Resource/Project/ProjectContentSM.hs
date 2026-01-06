module Wizard.Api.Resource.Project.ProjectContentSM where

import qualified Data.Map.Strict as M
import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.Comment.ProjectCommentThreadListSM ()
import Wizard.Api.Resource.Project.Event.ProjectEventListSM ()
import Wizard.Api.Resource.Project.ProjectContentDTO
import Wizard.Api.Resource.Project.ProjectContentJM ()
import Wizard.Api.Resource.Project.ProjectReplySM ()
import Wizard.Api.Resource.Project.Version.ProjectVersionListSM ()
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Service.Project.ProjectMapper

instance ToSchema ProjectContentDTO where
  declareNamedSchema = toSwagger (toContentDTO project1Ctn M.empty [] [])
