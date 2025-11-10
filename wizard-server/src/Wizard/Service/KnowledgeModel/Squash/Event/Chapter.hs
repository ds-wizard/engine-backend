module Wizard.Service.KnowledgeModel.Squash.Event.Chapter where

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Chapter.ChapterEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditChapterEvent where
  isSimpleEventSquashApplicable = not . isChanged questionUuids
  isReorderEventSquashApplicable (previousEvent, _) (newEvent, _) = previousEvent.entityUuid == newEvent.entityUuid
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) =
    createSquashedEvent oldEvent newEvent $
      EditChapterEvent'
        EditChapterEvent
          { title = applyValue oldContent newContent (.title)
          , text = applyValue oldContent newContent (.text)
          , annotations = applyValue oldContent newContent (.annotations)
          , questionUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.questionUuids)
          }
