module Wizard.Api.Resource.Project.File.ProjectFileSimpleSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.File.ProjectFileSimpleJM ()
import Wizard.Database.Migration.Development.Project.Data.ProjectFiles
import Wizard.Model.Project.File.ProjectFileSimple

instance ToSchema ProjectFileSimple where
  declareNamedSchema = toSwagger projectFileSimple
