module Wizard.Model.Questionnaire.QuestionnaireDetail where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Questionnaire.QuestionnairePermDTO
import Wizard.Model.Questionnaire.Questionnaire

data QuestionnaireDetail = QuestionnaireDetail
  { uuid :: U.UUID
  , name :: String
  , visibility :: QuestionnaireVisibility
  , sharing :: QuestionnaireSharing
  , packageId :: String
  , selectedQuestionTagUuids :: [U.UUID]
  , isTemplate :: Bool
  , migrationUuid :: Maybe U.UUID
  , permissions :: [QuestionnairePermDTO]
  , questionnaireActionsAvailable :: Int
  , questionnaireImportersAvailable :: Int
  , fileCount :: Int
  }
  deriving (Show, Eq, Generic)
