module Wizard.Model.Branch.BranchData where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.QuestionnaireReply
import WizardLib.KnowledgeModel.Model.Event.Event

data BranchData = BranchData
  { branchUuid :: U.UUID
  , metamodelVersion :: Int
  , events :: [Event]
  , replies :: M.Map String Reply
  , squashed :: Bool
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
