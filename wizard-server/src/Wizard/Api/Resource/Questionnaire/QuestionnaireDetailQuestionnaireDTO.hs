module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailQuestionnaireDTO where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Questionnaire.QuestionnairePermDTO
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireFileSimple
import Wizard.Model.Questionnaire.QuestionnaireReply
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

data QuestionnaireDetailQuestionnaireDTO = QuestionnaireDetailQuestionnaireDTO
  { uuid :: U.UUID
  , name :: String
  , visibility :: QuestionnaireVisibility
  , sharing :: QuestionnaireSharing
  , packageId :: String
  , selectedQuestionTagUuids :: [U.UUID]
  , isTemplate :: Bool
  , knowledgeModel :: KnowledgeModel
  , replies :: M.Map String Reply
  , labels :: M.Map String [U.UUID]
  , phaseUuid :: Maybe U.UUID
  , migrationUuid :: Maybe U.UUID
  , permissions :: [QuestionnairePermDTO]
  , files :: [QuestionnaireFileSimple]
  , unresolvedCommentCounts :: M.Map String (M.Map U.UUID Int)
  , resolvedCommentCounts :: M.Map String (M.Map U.UUID Int)
  , questionnaireActionsAvailable :: Int
  , questionnaireImportersAvailable :: Int
  , fileCount :: Int
  }
  deriving (Show, Eq, Generic)
