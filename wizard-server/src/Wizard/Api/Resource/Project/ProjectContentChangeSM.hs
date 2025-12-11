module Wizard.Api.Resource.Project.ProjectContentChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.Event.ProjectEventChangeSM ()
import Wizard.Api.Resource.Project.ProjectContentChangeDTO
import Wizard.Api.Resource.Project.ProjectContentChangeJM ()
import Wizard.Database.Migration.Development.Project.Data.Projects

instance ToSchema ProjectContentChangeDTO where
  declareNamedSchema = toSwagger contentChangeDTO
