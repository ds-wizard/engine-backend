module Wizard.Api.Resource.Questionnaire.QuestionnaireSettingsChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Questionnaire.QuestionnaireSettingsChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireSettingsChangeJM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires

instance ToSchema QuestionnaireSettingsChangeDTO where
  declareNamedSchema = toSwagger questionnaire1SettingsChange
