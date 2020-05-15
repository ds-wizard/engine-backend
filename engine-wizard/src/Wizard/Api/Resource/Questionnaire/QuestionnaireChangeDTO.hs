module Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Questionnaire.QuestionnaireLabelDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyDTO
import Wizard.Model.Questionnaire.Questionnaire

data QuestionnaireChangeDTO =
  QuestionnaireChangeDTO
    { _questionnaireChangeDTOName :: String
    , _questionnaireChangeDTOAccessibility :: QuestionnaireAccessibility
    , _questionnaireChangeDTOLevel :: Int
    , _questionnaireChangeDTOReplies :: [ReplyDTO]
    , _questionnaireChangeDTOLabels :: [LabelDTO]
    , _questionnaireChangeDTOTemplateUuid :: Maybe U.UUID
    }
  deriving (Show, Eq, Generic)
