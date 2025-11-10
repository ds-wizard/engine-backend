module Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorEvent where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent

data KnowledgeModelEditorEvent = KnowledgeModelEditorEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , content :: KnowledgeModelEventData
  , knowledgeModelEditorUuid :: U.UUID
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
