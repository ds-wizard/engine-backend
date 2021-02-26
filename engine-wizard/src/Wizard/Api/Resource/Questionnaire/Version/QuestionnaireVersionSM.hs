module Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionJM ()
import Wizard.Api.Resource.User.UserSuggestionSM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireVersions

instance ToSchema QuestionnaireVersionDTO where
  declareNamedSchema = simpleToSchema questionnaireVersion1Dto
