module Wizard.Api.Resource.Project.ProjectContentChangeDTO where

import GHC.Generics

import Wizard.Api.Resource.Project.Event.ProjectEventChangeDTO

data ProjectContentChangeDTO = ProjectContentChangeDTO
  { events :: [ProjectEventChangeDTO]
  }
  deriving (Show, Eq, Generic)
