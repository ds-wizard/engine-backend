module Api.Resource.Questionnaire.QuestionnaireCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

import Model.Questionnaire.Questionnaire

data QuestionnaireCreateDTO = QuestionnaireCreateDTO
  { _questionnaireCreateDTOName :: String
  , _questionnaireCreateDTOPackageId :: String
  , _questionnaireCreateDTOAccessibility :: QuestionnaireAccessibility
  , _questionnaireCreateDTOTagUuids :: [U.UUID]
  } deriving (Show, Eq, Generic)
