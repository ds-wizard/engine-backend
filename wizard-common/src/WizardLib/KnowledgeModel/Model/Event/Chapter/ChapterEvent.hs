module WizardLib.KnowledgeModel.Model.Event.Chapter.ChapterEvent where

import Data.Hashable
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Model.Common.MapEntry
import WizardLib.Common.Util.Hashable ()
import WizardLib.KnowledgeModel.Model.Event.EventField

data AddChapterEvent = AddChapterEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , title :: String
  , text :: Maybe String
  , annotations :: [MapEntry String String]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable AddChapterEvent

data EditChapterEvent = EditChapterEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , title :: EventField String
  , text :: EventField (Maybe String)
  , annotations :: EventField [MapEntry String String]
  , questionUuids :: EventField [U.UUID]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable EditChapterEvent

data DeleteChapterEvent = DeleteChapterEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable DeleteChapterEvent
