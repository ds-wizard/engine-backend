module Wizard.Model.Questionnaire.QuestionnaireDetailQuestionnaire where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Questionnaire.QuestionnairePermDTO
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireEvent

data QuestionnaireDetailQuestionnaire = QuestionnaireDetailQuestionnaire
  { uuid :: U.UUID
  , name :: String
  , visibility :: QuestionnaireVisibility
  , sharing :: QuestionnaireSharing
  , packageId :: String
  , selectedQuestionTagUuids :: [U.UUID]
  , isTemplate :: Bool
  , migrationUuid :: Maybe U.UUID
  , permissions :: [QuestionnairePermDTO]
  , events :: [QuestionnaireEvent]
  , questionnaireActionsAvailable :: Int
  , questionnaireImportersAvailable :: Int
  }
  deriving (Show, Eq, Generic)
