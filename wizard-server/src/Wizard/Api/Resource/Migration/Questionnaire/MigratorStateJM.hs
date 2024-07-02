module Wizard.Api.Resource.Migration.Questionnaire.MigratorStateJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailQuestionnaireJM ()

instance FromJSON MigratorStateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON MigratorStateDTO where
  toJSON = genericToJSON jsonOptions
