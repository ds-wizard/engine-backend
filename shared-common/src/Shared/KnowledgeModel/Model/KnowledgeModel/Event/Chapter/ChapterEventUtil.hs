module Shared.KnowledgeModel.Model.KnowledgeModel.Event.Chapter.ChapterEventUtil where

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Chapter.ChapterEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Common.CommonUtil
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField

instance IsEmptyEvent EditChapterEvent where
  isEmptyEvent event =
    or
      [ isChangedValue event.title
      , isChangedValue event.text
      , isChangedValue event.annotations
      , isChangedValue event.questionUuids
      ]
