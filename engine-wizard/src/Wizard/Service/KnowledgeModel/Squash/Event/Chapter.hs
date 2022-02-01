module Wizard.Service.KnowledgeModel.Squash.Event.Chapter where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Event.Chapter.ChapterEvent
import Shared.Model.Event.EventField
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditChapterEvent where
  isSimpleEventSquashApplicable = not . isChanged questionUuids
  isTypeChanged _ _ = False
  simpleSquashEvent oldEvent newEvent =
    EditChapterEvent
      { _editChapterEventUuid = newEvent ^. uuid
      , _editChapterEventParentUuid = newEvent ^. parentUuid
      , _editChapterEventEntityUuid = newEvent ^. entityUuid
      , _editChapterEventTitle = applyValue oldEvent newEvent title
      , _editChapterEventText = applyValue oldEvent newEvent text
      , _editChapterEventAnnotations = applyValue oldEvent newEvent annotations
      , _editChapterEventQuestionUuids = NothingChanged
      , _editChapterEventCreatedAt = newEvent ^. createdAt
      }
