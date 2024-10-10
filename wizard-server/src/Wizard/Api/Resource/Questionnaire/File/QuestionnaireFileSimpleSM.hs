module Wizard.Api.Resource.Questionnaire.File.QuestionnaireFileSimpleSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Questionnaire.File.QuestionnaireFileSimpleJM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireFiles
import Wizard.Model.Questionnaire.QuestionnaireFileSimple

instance ToSchema QuestionnaireFileSimple where
  declareNamedSchema = toSwagger questionnaireFileSimple
