module Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionListSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionListJM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireVersions
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Model.Questionnaire.QuestionnaireVersionList
import WizardLib.Public.Api.Resource.User.UserSuggestionSM ()

instance ToSchema QuestionnaireVersionList where
  declareNamedSchema = toSwagger (questionnaireVersion1List questionnaire1Uuid)
