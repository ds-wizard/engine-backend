module Wizard.Api.Resource.TypeHint.TypeHintRequestDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent

data TypeHintLegacyRequestDTO = TypeHintLegacyRequestDTO
  { knowledgeModelPackageId :: Maybe String
  , events :: [KnowledgeModelEvent]
  , questionUuid :: U.UUID
  , q :: String
  }
  deriving (Show, Eq, Generic)

data TypeHintRequestDTO
  = KnowledgeModelEditorIntegrationTypeHintRequest' KnowledgeModelEditorIntegrationTypeHintRequest
  | KnowledgeModelEditorQuestionTypeHintRequest' KnowledgeModelEditorQuestionTypeHintRequest
  | QuestionnaireTypeHintRequest' QuestionnaireTypeHintRequest
  deriving (Show, Eq, Generic)

data KnowledgeModelEditorIntegrationTypeHintRequest = KnowledgeModelEditorIntegrationTypeHintRequest
  { knowledgeModelEditorUuid :: U.UUID
  , integrationUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

data KnowledgeModelEditorQuestionTypeHintRequest = KnowledgeModelEditorQuestionTypeHintRequest
  { knowledgeModelEditorUuid :: U.UUID
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
