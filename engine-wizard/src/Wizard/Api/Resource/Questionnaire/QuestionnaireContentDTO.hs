module Wizard.Api.Resource.Questionnaire.QuestionnaireContentDTO where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Model.Questionnaire.QuestionnaireReply

data QuestionnaireContentDTO =
  QuestionnaireContentDTO
    { _questionnaireContentDTOLevel :: Int
    , _questionnaireContentDTOReplies :: M.Map String Reply
    , _questionnaireContentDTOLabels :: M.Map String [U.UUID]
    , _questionnaireContentDTOEvents :: [QuestionnaireEventDTO]
    , _questionnaireContentDTOVersions :: [QuestionnaireVersionDTO]
    }
  deriving (Show, Eq, Generic)
