module Wizard.Api.Resource.Project.Version.ProjectVersionListSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.Version.ProjectVersionListJM ()
import Wizard.Database.Migration.Development.Project.Data.ProjectVersions
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Model.Project.Version.ProjectVersionList
import WizardLib.Public.Api.Resource.User.UserSuggestionSM ()

instance ToSchema ProjectVersionList where
  declareNamedSchema = toSwagger (projectVersion1List project1Uuid)
