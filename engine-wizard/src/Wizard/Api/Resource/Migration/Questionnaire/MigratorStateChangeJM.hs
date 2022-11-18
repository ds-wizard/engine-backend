module Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailJM ()

instance FromJSON MigratorStateChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON MigratorStateChangeDTO where
  toJSON = genericToJSON jsonOptions
