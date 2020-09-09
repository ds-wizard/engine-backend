module Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeDTO where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyDTO

data QuestionnaireContentChangeDTO =
  QuestionnaireContentChangeDTO
    { _questionnaireContentChangeDTOLevel :: Int
    , _questionnaireContentChangeDTOReplies :: M.Map String ReplyValueDTO
    , _questionnaireContentChangeDTOLabels :: M.Map String [U.UUID]
    }
  deriving (Show, Eq, Generic)
