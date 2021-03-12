module Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeJM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireVersions

instance ToSchema QuestionnaireVersionChangeDTO where
  declareNamedSchema = simpleToSchema questionnaireVersion1ChangeDto
