module Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.Questionnaire

data QuestionnaireCreateDTO =
  QuestionnaireCreateDTO
    { _questionnaireCreateDTOName :: String
    , _questionnaireCreateDTOPackageId :: String
    , _questionnaireCreateDTOVisibility :: QuestionnaireVisibility
    , _questionnaireCreateDTOSharing :: QuestionnaireSharing
    , _questionnaireCreateDTOQuestionTagUuids :: [U.UUID]
    , _questionnaireCreateDTOTemplateId :: Maybe String
    , _questionnaireCreateDTOFormatUuid :: Maybe U.UUID
    }
  deriving (Show, Eq, Generic)
