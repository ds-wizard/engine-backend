module KMMigration.Migration.Event.Chapter.AddChapterEvent where

import Control.Lens

import KMMigration.Migration.Event.Common
import KMMigration.Model.Common
import KMMigration.Model.KnowledgeModel

data AddChapterEvent = AddChapterEvent
  { _achUuid :: UUID
  , _achKmUuid :: UUID
  , _achChapterUuid :: UUID
  , _achTitle :: String
  , _achText :: String
  }

makeLenses ''AddChapterEvent

instance SameUuid AddChapterEvent Chapter where
  equalsUuid e ch = ch ^. chUuid == e ^. achChapterUuid
