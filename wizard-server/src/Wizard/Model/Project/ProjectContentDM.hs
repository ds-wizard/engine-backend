module Wizard.Model.Project.ProjectContentDM where

import qualified Data.Map.Strict as M

import Wizard.Model.Project.ProjectContent

defaultProjectContent :: ProjectContent
defaultProjectContent =
  ProjectContent
    { phaseUuid = Nothing
    , replies = M.empty
    , labels = M.empty
    }
