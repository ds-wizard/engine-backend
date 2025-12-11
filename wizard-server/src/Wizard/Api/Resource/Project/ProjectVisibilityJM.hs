module Wizard.Api.Resource.Project.ProjectVisibilityJM where

import Data.Aeson

import Wizard.Model.Project.Project

instance FromJSON ProjectVisibility

instance ToJSON ProjectVisibility
