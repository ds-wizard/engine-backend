module Shared.KnowledgeModel.Model.KnowledgeModel.Event.Choice.ChoiceEvent where

import Data.Hashable
import GHC.Generics

import Shared.Common.Model.Common.MapEntry
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.KnowledgeModel.Util.Hashable ()

data AddChoiceEvent = AddChoiceEvent
  { aLabel :: String
  , annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable AddChoiceEvent

data EditChoiceEvent = EditChoiceEvent
  { aLabel :: EventField String
  , annotations :: EventField [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable EditChoiceEvent

data DeleteChoiceEvent = DeleteChoiceEvent
  deriving (Show, Eq, Generic)

instance Hashable DeleteChoiceEvent
