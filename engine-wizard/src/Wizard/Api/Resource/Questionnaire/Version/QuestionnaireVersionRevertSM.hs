module Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionRevertSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionRevertDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionRevertJM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireVersions

instance ToSchema QuestionnaireVersionRevertDTO where
  declareNamedSchema = simpleToSchema questionnaireVersion1RevertDto
