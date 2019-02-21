module Api.Resource.Questionnaire.QuestionnaireCreateDTO where

import qualified Data.UUID as U

data QuestionnaireCreateDTO = QuestionnaireCreateDTO
  { _questionnaireCreateDTOName :: String
  , _questionnaireCreateDTOPackageId :: String
  , _questionnaireCreateDTOPrivate :: Bool
  , _questionnaireCreateDTOTagUuids :: [U.UUID]
  }
