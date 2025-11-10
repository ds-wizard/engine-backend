module Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelRawEvent where

import Data.Aeson
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data KnowledgeModelRawEvent = KnowledgeModelRawEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , content :: Value
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
