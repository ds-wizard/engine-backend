module Wizard.Api.Resource.TypeHint.TypeHintRequestDTO where

import qualified Data.UUID as U
import GHC.Generics

import WizardLib.KnowledgeModel.Model.Event.Event

data TypeHintLegacyRequestDTO = TypeHintLegacyRequestDTO
  { packageId :: Maybe String
  , events :: [Event]
  , questionUuid :: U.UUID
  , q :: String
  }
  deriving (Show, Eq, Generic)

data TypeHintRequestDTO
  = BranchIntegrationTypeHintRequest' BranchIntegrationTypeHintRequest
  | BranchQuestionTypeHintRequest' BranchQuestionTypeHintRequest
  | QuestionnaireTypeHintRequest' QuestionnaireTypeHintRequest
  deriving (Show, Eq, Generic)

data BranchIntegrationTypeHintRequest = BranchIntegrationTypeHintRequest
  { branchUuid :: U.UUID
  , integrationUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

data BranchQuestionTypeHintRequest = BranchQuestionTypeHintRequest
  { branchUuid :: U.UUID
  , questionUuid :: U.UUID
  , q :: String
  }
  deriving (Show, Eq, Generic)

data QuestionnaireTypeHintRequest = QuestionnaireTypeHintRequest
  { questionnaireUuid :: U.UUID
  , questionUuid :: U.UUID
  , q :: String
  }
  deriving (Show, Eq, Generic)
