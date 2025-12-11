module Wizard.Api.Resource.Project.ProjectContentDTO where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Project.Comment.ProjectCommentList
import Wizard.Model.Project.Event.ProjectEventList
import Wizard.Model.Project.ProjectReply
import Wizard.Model.Project.Version.ProjectVersionList

data ProjectContentDTO = ProjectContentDTO
  { phaseUuid :: Maybe U.UUID
  , replies :: M.Map String Reply
  , commentThreadsMap :: M.Map String [ProjectCommentThreadList]
  , labels :: M.Map String [U.UUID]
  , events :: [ProjectEventList]
  , versions :: [ProjectVersionList]
  }
  deriving (Show, Eq, Generic)
