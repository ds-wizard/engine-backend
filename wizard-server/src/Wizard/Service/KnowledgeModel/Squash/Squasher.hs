module Wizard.Service.KnowledgeModel.Squash.Squasher where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Util.List (groupBy, replace)
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Wizard.Service.KnowledgeModel.Squash.Event.Event

instance Ord KnowledgeModelEvent where
  compare a b = compare a.createdAt b.createdAt

squash :: [KnowledgeModelEvent] -> [KnowledgeModelEvent]
squash events =
  let groupedEvents = groupBy (\e1 e2 -> utctDay (e1.createdAt) == utctDay (e2.createdAt)) events
      squashedEvents = fmap (squashReorderEvents . squashSimple) groupedEvents
   in concat squashedEvents

squashSimple :: [KnowledgeModelEvent] -> [KnowledgeModelEvent]
squashSimple events =
  let (entities, squashedEvents) = foldl go (M.empty, []) events
   in reverse squashedEvents
  where
    deleteEvent :: KnowledgeModelEvent -> [KnowledgeModelEvent] -> [KnowledgeModelEvent]
    deleteEvent = L.deleteBy (\e1 e2 -> e1.uuid == e2.uuid)
    go :: (M.Map U.UUID KnowledgeModelEvent, [KnowledgeModelEvent]) -> KnowledgeModelEvent -> (M.Map U.UUID KnowledgeModelEvent, [KnowledgeModelEvent])
    go (entities, events) newEvent =
      case M.lookup newEvent.entityUuid entities of
        Just oldEvent ->
          if isTypeChanged' oldEvent newEvent || not (isSimpleEventSquashApplicable' newEvent)
            then
              let entities' = M.delete oldEvent.entityUuid entities
                  events' = newEvent : events
               in (entities', events')
            else
              let squashedEvent = simpleSquashEvent' Nothing oldEvent newEvent
                  entities' = M.insert newEvent.entityUuid squashedEvent entities
                  events' = replace squashedEvent oldEvent events
               in (entities', events')
        Nothing ->
          if isSimpleEventSquashApplicable' newEvent
            then
              let entities' = M.insert newEvent.entityUuid newEvent entities
                  events' = newEvent : events
               in (entities', events')
            else
              let entities' = entities
                  events' = newEvent : events
               in (entities', events')

squashReorderEvents :: [KnowledgeModelEvent] -> [KnowledgeModelEvent]
squashReorderEvents events =
  let (eventsToDeleted, squashedEvents, _) = foldl go ([], [], Nothing) events
   in foldr deleteEvent (reverse squashedEvents) eventsToDeleted
  where
    deleteEvent :: KnowledgeModelEvent -> [KnowledgeModelEvent] -> [KnowledgeModelEvent]
    deleteEvent = L.deleteBy (\e1 e2 -> e1.uuid == e2.uuid)
    go :: ([KnowledgeModelEvent], [KnowledgeModelEvent], Maybe KnowledgeModelEvent) -> KnowledgeModelEvent -> ([KnowledgeModelEvent], [KnowledgeModelEvent], Maybe KnowledgeModelEvent)
    go (eventsToDeleted, events, mPreviousEvent) newEvent =
      case mPreviousEvent of
        Just previousEvent ->
          if isTypeChanged' previousEvent newEvent || not (isReorderEventSquashApplicable' previousEvent newEvent)
            then
              let eventsToDeleted' = eventsToDeleted
                  events' = newEvent : events
               in (eventsToDeleted', events', Just newEvent)
            else
              let squashedEvent = simpleSquashEvent' mPreviousEvent previousEvent newEvent
                  eventsToDeleted' = previousEvent : eventsToDeleted
                  events' = squashedEvent : events
               in (eventsToDeleted', events', Just squashedEvent)
        Nothing ->
          let eventsToDeleted' = eventsToDeleted
              events' = newEvent : events
           in (eventsToDeleted', events', Just newEvent)
