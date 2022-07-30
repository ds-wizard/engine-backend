module Wizard.Api.Resource.Questionnaire.QuestionnaireLabelListSM where

import Control.Lens ((^.))
import Data.Swagger

import LensesConfig
import Shared.Util.Swagger
import Wizard.Api.Resource.Questionnaire.QuestionnaireLabelListDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireLabelListJM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Service.Questionnaire.QuestionnaireMapper

instance ToSchema QuestionnaireLabelListDTO where
  declareNamedSchema = simpleToSchema (toLabelListDTO (questionnaire1Ctn ^. labels))
