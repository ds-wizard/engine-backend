module Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.Questionnaire

data QuestionnaireCreateDTO =
  QuestionnaireCreateDTO
    { _questionnaireCreateDTOName :: String
    , _questionnaireCreateDTOPackageId :: String
    , _questionnaireCreateDTOVisibility :: QuestionnaireVisibility
    , _questionnaireCreateDTOTagUuids :: [U.UUID]
    , _questionnaireCreateDTOTemplateId :: Maybe String
    }
  deriving (Show, Eq, Generic)
