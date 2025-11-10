module Wizard.Api.Resource.Questionnaire.Migration.MigratorStateChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Questionnaire.Migration.MigratorStateChangeDTO
import Wizard.Api.Resource.Questionnaire.Migration.MigratorStateChangeJM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.MigratorStates

instance ToSchema MigratorStateChangeDTO where
  declareNamedSchema = toSwagger migratorStateChange
