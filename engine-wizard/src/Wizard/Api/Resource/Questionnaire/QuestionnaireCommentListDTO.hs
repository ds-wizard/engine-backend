module Wizard.Api.Resource.Questionnaire.QuestionnaireCommentListDTO where

import qualified Data.Map.Strict as M
import GHC.Generics

import Wizard.Model.Questionnaire.QuestionnaireComment

data QuestionnaireCommentListDTO =
  QuestionnaireCommentListDTO
    { _questionnaireCommentListDTOCommentThreadsMap :: M.Map String [QuestionnaireCommentThread]
    }
  deriving (Show, Eq, Generic)
