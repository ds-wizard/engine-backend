module Wizard.Api.Resource.Questionnaire.QuestionnaireCommentListSM where

import Control.Lens ((^.))
import Data.Swagger

import LensesConfig
import Shared.Util.Swagger
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentListDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentListJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentSM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Service.Questionnaire.QuestionnaireMapper

instance ToSchema QuestionnaireCommentListDTO where
  declareNamedSchema = simpleToSchema (toCommentListDTO (questionnaire1Ctn ^. commentThreadsMap))
