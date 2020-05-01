module Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeJM ()
import Wizard.Database.Migration.Development.Migration.Questionnaire.Data.MigratorStates

instance ToSchema MigratorStateChangeDTO where
  declareNamedSchema = simpleToSchema migratorStateChange
