module Wizard.Api.Resource.Questionnaire.Migration.MigratorStateJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Questionnaire.Migration.MigratorStateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailQuestionnaireJM ()

instance FromJSON MigratorStateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON MigratorStateDTO where
  toJSON = genericToJSON jsonOptions
