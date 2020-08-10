module Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeDTO where

import GHC.Generics

import Wizard.Api.Resource.Questionnaire.QuestionnaireLabelDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyDTO

data QuestionnaireContentChangeDTO =
  QuestionnaireContentChangeDTO
    { _questionnaireContentChangeDTOLevel :: Int
    , _questionnaireContentChangeDTOReplies :: [ReplyDTO]
    , _questionnaireContentChangeDTOLabels :: [LabelDTO]
    }
  deriving (Show, Eq, Generic)
