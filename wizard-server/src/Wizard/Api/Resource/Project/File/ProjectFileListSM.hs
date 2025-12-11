module Wizard.Api.Resource.Project.File.ProjectFileListSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.File.ProjectFileListJM ()
import Wizard.Api.Resource.Project.ProjectSimpleSM ()
import Wizard.Database.Migration.Development.Project.Data.ProjectFiles
import Wizard.Model.Project.File.ProjectFileList
import WizardLib.Public.Api.Resource.User.UserSuggestionSM ()

instance ToSchema ProjectFileList where
  declareNamedSchema = toSwagger projectFileList
