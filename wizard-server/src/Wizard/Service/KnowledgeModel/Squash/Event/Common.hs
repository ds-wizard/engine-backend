module Wizard.Service.KnowledgeModel.Squash.Event.Common where

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField

applyValue :: eventContent -> eventContent -> (eventContent -> EventField value) -> EventField value
applyValue oldContent newContent getter =
  case getter newContent of
    (ChangedValue value) -> ChangedValue value
    NothingChanged -> getter oldContent

class SimpleEventSquash content where
  isSimpleEventSquashApplicable :: content -> Bool
  isReorderEventSquashApplicable :: (KnowledgeModelEvent, content) -> (KnowledgeModelEvent, content) -> Bool
  isTypeChanged :: content -> content -> Bool
  simpleSquashEvent :: Maybe KnowledgeModelEvent -> (KnowledgeModelEvent, content) -> (KnowledgeModelEvent, content) -> KnowledgeModelEvent

isChanged getter event =
  case getter event of
    (ChangedValue _) -> True
    NothingChanged -> False

applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) accessor =
  case mPreviousEvent of
    Just previousEvent ->
      if previousEvent.entityUuid == newEvent.entityUuid
        then applyValue oldContent newContent accessor
        else NothingChanged
    _ -> NothingChanged

createSquashedEvent :: KnowledgeModelEvent -> KnowledgeModelEvent -> KnowledgeModelEventData -> KnowledgeModelEvent
createSquashedEvent oldEvent newEvent content =
  KnowledgeModelEvent
    { uuid = newEvent.uuid
    , parentUuid = newEvent.parentUuid
    , entityUuid = newEvent.entityUuid
    , content = content
    , createdAt = oldEvent.createdAt
    }
