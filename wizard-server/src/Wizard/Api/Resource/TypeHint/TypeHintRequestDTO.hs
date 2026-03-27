module Wizard.Api.Resource.TypeHint.TypeHintRequestDTO where

import qualified Data.UUID as U
import GHC.Generics

data TypeHintRequestDTO
  = KnowledgeModelEditorIntegrationTypeHintRequest' KnowledgeModelEditorIntegrationTypeHintRequest
  | KnowledgeModelEditorQuestionTypeHintRequest' KnowledgeModelEditorQuestionTypeHintRequest
  | ProjectTypeHintRequest' ProjectTypeHintRequest
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

data ProjectTypeHintRequest = ProjectTypeHintRequest
  { projectUuid :: U.UUID
  , questionUuid :: U.UUID
  , q :: String
  }
  deriving (Show, Eq, Generic)
