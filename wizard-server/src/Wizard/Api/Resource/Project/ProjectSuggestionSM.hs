module Wizard.Api.Resource.Project.ProjectSuggestionSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.ProjectSuggestionJM ()
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Model.Project.ProjectSuggestion
import Wizard.Service.Project.ProjectMapper

instance ToSchema ProjectSuggestion where
  declareNamedSchema = toSwagger (toSuggestion project1)
