module Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorRawEvent where

import Data.Aeson
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data KnowledgeModelEditorRawEvent = KnowledgeModelEditorRawEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , content :: Value
  , knowledgeModelEditorUuid :: U.UUID
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
