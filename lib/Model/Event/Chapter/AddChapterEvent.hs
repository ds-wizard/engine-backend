module Model.Event.Chapter.AddChapterEvent where

import Control.Lens
import Data.UUID
import GHC.Generics

import Model.Common
import Model.KnowledgeModel.KnowledgeModel

data AddChapterEvent = AddChapterEvent
  { _achUuid :: UUID
  , _achKmUuid :: UUID
  , _achChapterUuid :: UUID
  , _achTitle :: String
  , _achText :: String
  } deriving (Show, Eq, Generic)

makeLenses ''AddChapterEvent

instance SameUuid AddChapterEvent Chapter where
  equalsUuid e ch = ch ^. chUuid == e ^. achChapterUuid
