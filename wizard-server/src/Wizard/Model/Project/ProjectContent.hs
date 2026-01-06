module Wizard.Model.Project.ProjectContent where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Project.ProjectReply

data ProjectContent = ProjectContent
  { phaseUuid :: Maybe U.UUID
  , replies :: M.Map String Reply
  , labels :: M.Map String [U.UUID]
  }
  deriving (Generic, Eq, Show)
