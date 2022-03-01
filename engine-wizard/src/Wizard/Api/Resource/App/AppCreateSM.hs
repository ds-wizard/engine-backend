module Wizard.Api.Resource.App.AppCreateSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.App.AppCreateDTO
import Wizard.Api.Resource.App.AppCreateJM ()
import Wizard.Database.Migration.Development.App.Data.Apps

instance ToSchema AppCreateDTO where
  declareNamedSchema = simpleToSchema appCreateDto
