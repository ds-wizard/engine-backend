module Api.Resource.Questionnaire.QuestionnaireCreateDTO where

import qualified Data.UUID as U

import Model.Questionnaire.Questionnaire

data QuestionnaireCreateDTO = QuestionnaireCreateDTO
  { _questionnaireCreateDTOName :: String
  , _questionnaireCreateDTOPackageId :: String
  , _questionnaireCreateDTOAccessibility :: QuestionnaireAccessibility
  , _questionnaireCreateDTOTagUuids :: [U.UUID]
  }
