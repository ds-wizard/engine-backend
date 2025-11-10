module Shared.KnowledgeModel.Model.KnowledgeModel.Event.Tag.TagEvent where

import Data.Hashable
import GHC.Generics

import Shared.Common.Model.Common.MapEntry
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.KnowledgeModel.Util.Hashable ()

data AddTagEvent = AddTagEvent
  { name :: String
  , description :: Maybe String
  , color :: String
  , annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable AddTagEvent

data EditTagEvent = EditTagEvent
  { name :: EventField String
  , description :: EventField (Maybe String)
  , color :: EventField String
  , annotations :: EventField [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable EditTagEvent

data DeleteTagEvent = DeleteTagEvent
  deriving (Show, Eq, Generic)

instance Hashable DeleteTagEvent
