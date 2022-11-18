module Shared.Model.Event.Chapter.ChapterEventUtil where

import Shared.Model.Event.Chapter.ChapterEvent
import Shared.Model.Event.Common.CommonUtil
import Shared.Model.Event.EventField

instance IsEmptyEvent EditChapterEvent where
  isEmptyEvent event =
    or
      [ isChangedValue event.title
      , isChangedValue event.text
      , isChangedValue event.annotations
      , isChangedValue event.questionUuids
      ]
