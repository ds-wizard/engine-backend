module Shared.KnowledgeModel.Model.KnowledgeModel.Event.Chapter.ChapterEvent where

import Data.Hashable
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Model.Common.MapEntry
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.KnowledgeModel.Util.Hashable ()

data AddChapterEvent = AddChapterEvent
  { title :: String
  , text :: Maybe String
  , annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable AddChapterEvent

data EditChapterEvent = EditChapterEvent
  { title :: EventField String
  , text :: EventField (Maybe String)
  , annotations :: EventField [MapEntry String String]
  , questionUuids :: EventField [U.UUID]
  }
  deriving (Show, Eq, Generic)

instance Hashable EditChapterEvent

data DeleteChapterEvent = DeleteChapterEvent
  deriving (Show, Eq, Generic)

instance Hashable DeleteChapterEvent
