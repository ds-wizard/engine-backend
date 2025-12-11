module Wizard.Api.Resource.Project.ProjectSharingJM where

import Data.Aeson

import Wizard.Model.Project.Project

instance FromJSON ProjectSharing

instance ToJSON ProjectSharing
