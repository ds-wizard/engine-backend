module Shared.KnowledgeModel.Model.KnowledgeModel.Event.Phase.PhaseEvent where

import Data.Hashable
import GHC.Generics

import Shared.Common.Model.Common.MapEntry
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.KnowledgeModel.Util.Hashable ()

data AddPhaseEvent = AddPhaseEvent
  { title :: String
  , description :: Maybe String
  , annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable AddPhaseEvent

data EditPhaseEvent = EditPhaseEvent
  { title :: EventField String
  , description :: EventField (Maybe String)
  , annotations :: EventField [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable EditPhaseEvent

data DeletePhaseEvent = DeletePhaseEvent
  deriving (Show, Eq, Generic)

instance Hashable DeletePhaseEvent
