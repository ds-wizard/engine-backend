module Wizard.Api.Resource.Project.ProjectStateJM where

import Data.Aeson

import Wizard.Model.Project.ProjectState

instance FromJSON ProjectState

instance ToJSON ProjectState
