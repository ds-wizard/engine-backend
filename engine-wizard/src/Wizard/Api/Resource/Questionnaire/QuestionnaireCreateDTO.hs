module Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.Questionnaire

data QuestionnaireCreateDTO =
  QuestionnaireCreateDTO
    { _questionnaireCreateDTOName :: String
    , _questionnaireCreateDTOPackageId :: String
    , _questionnaireCreateDTOAccessibility :: QuestionnaireAccessibility
    , _questionnaireCreateDTOTagUuids :: [U.UUID]
    , _questionnaireCreateDTOTemplateUuid :: Maybe U.UUID
    }
  deriving (Show, Eq, Generic)
