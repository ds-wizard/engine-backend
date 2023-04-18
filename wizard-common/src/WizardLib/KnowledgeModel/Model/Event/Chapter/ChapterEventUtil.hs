module WizardLib.KnowledgeModel.Model.Event.Chapter.ChapterEventUtil where

import WizardLib.KnowledgeModel.Model.Event.Chapter.ChapterEvent
import WizardLib.KnowledgeModel.Model.Event.Common.CommonUtil
import WizardLib.KnowledgeModel.Model.Event.EventField

instance IsEmptyEvent EditChapterEvent where
  isEmptyEvent event =
    or
      [ isChangedValue event.title
      , isChangedValue event.text
      , isChangedValue event.annotations
      , isChangedValue event.questionUuids
      ]
