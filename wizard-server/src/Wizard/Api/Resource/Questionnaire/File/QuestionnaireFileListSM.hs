module Wizard.Api.Resource.Questionnaire.File.QuestionnaireFileListSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Questionnaire.File.QuestionnaireFileListJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSimpleSM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireFiles
import Wizard.Model.Questionnaire.QuestionnaireFileList
import WizardLib.Public.Api.Resource.User.UserSuggestionSM ()

instance ToSchema QuestionnaireFileList where
  declareNamedSchema = toSwagger questionnaireFileList
