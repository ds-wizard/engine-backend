module KMMigration.Migration.Event.Chapter.EditChapterEvent where

import Control.Lens

import KMMigration.Migration.Event.Common
import KMMigration.Model.Common
import KMMigration.Model.KnowledgeModel

data EditChapterEvent = EditChapterEvent
  { _echUuid :: UUID
  , _echKmUuid :: UUID
  , _echChapterUuid :: UUID
  , _echTitle :: Maybe String
  , _echText :: Maybe String
  , _echQuestionIds :: Maybe [UUID]
  }

makeLenses ''EditChapterEvent

instance SameUuid EditChapterEvent Chapter where
  equalsUuid e ch = ch ^. chUuid == e ^. echChapterUuid
