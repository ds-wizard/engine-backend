module Wizard.Api.Resource.Questionnaire.QuestionnairePermChangeDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.QuestionnairePerm

data QuestionnairePermChangeDTO = QuestionnairePermChangeDTO
  { memberType :: QuestionnairePermType
  , memberUuid :: U.UUID
  , perms :: [String]
  }
  deriving (Show, Eq, Generic)
