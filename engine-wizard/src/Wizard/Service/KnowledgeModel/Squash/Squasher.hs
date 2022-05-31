module Wizard.Service.KnowledgeModel.Squash.Squasher where

import Control.Lens ((^.))
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Shared.Model.Common.Lens
import Shared.Model.Event.Event
import Shared.Model.Event.EventLenses ()
import Shared.Util.List (groupBy)
import Wizard.Service.KnowledgeModel.Squash.Event.Common
import Wizard.Service.KnowledgeModel.Squash.Event.Event ()

instance Ord Event where
  compare a b = compare (a ^. createdAt') (b ^. createdAt')

squash :: [Event] -> [Event]
squash events =
  let groupedEvents = groupBy (\e1 e2 -> utctDay (e1 ^. createdAt') == utctDay (e2 ^. createdAt')) events
      squashedEvents = fmap (squashReorderEvents . squashSimple) groupedEvents
   in concat squashedEvents

squashSimple :: [Event] -> [Event]
squashSimple events =
  let (entities, eventsToDeleted, squashedEvents) = foldl go (M.empty, [], []) events
   in foldr deleteEvent (reverse squashedEvents) eventsToDeleted
  where
    deleteEvent :: Event -> [Event] -> [Event]
    deleteEvent = L.deleteBy (\e1 e2 -> e1 ^. uuid' == e2 ^. uuid')
    go :: (M.Map U.UUID Event, [Event], [Event]) -> Event -> (M.Map U.UUID Event, [Event], [Event])
    go (entities, eventsToDeleted, events) newEvent =
      case M.lookup (newEvent ^. entityUuid') entities of
        Just oldEvent ->
          if isTypeChanged oldEvent newEvent || not (isSimpleEventSquashApplicable newEvent)
            then let entities' = M.delete (oldEvent ^. entityUuid') entities
                     eventsToDeleted' = eventsToDeleted
                     events' = newEvent : events
                  in (entities', eventsToDeleted', events')
            else let squashedEvent = simpleSquashEvent Nothing oldEvent newEvent
                     entities' = M.insert (newEvent ^. entityUuid') squashedEvent entities
                     eventsToDeleted' = oldEvent : eventsToDeleted
                     events' = squashedEvent : events
                  in (entities', eventsToDeleted', events')
        Nothing ->
          if isSimpleEventSquashApplicable newEvent
            then let entities' = M.insert (newEvent ^. entityUuid') newEvent entities
                     eventsToDeleted' = eventsToDeleted
                     events' = newEvent : events
                  in (entities', eventsToDeleted', events')
            else let entities' = entities
                     eventsToDeleted' = eventsToDeleted
                     events' = newEvent : events
                  in (entities', eventsToDeleted', events')

squashReorderEvents :: [Event] -> [Event]
squashReorderEvents events =
  let (eventsToDeleted, squashedEvents, _) = foldl go ([], [], Nothing) events
   in foldr deleteEvent (reverse squashedEvents) eventsToDeleted
  where
    deleteEvent :: Event -> [Event] -> [Event]
    deleteEvent = L.deleteBy (\e1 e2 -> e1 ^. uuid' == e2 ^. uuid')
    go :: ([Event], [Event], Maybe Event) -> Event -> ([Event], [Event], Maybe Event)
    go (eventsToDeleted, events, mPreviousEvent) newEvent =
      case mPreviousEvent of
        Just previousEvent ->
          if isTypeChanged previousEvent newEvent || not (isReorderEventSquashApplicable previousEvent newEvent)
            then let eventsToDeleted' = eventsToDeleted
                     events' = newEvent : events
                  in (eventsToDeleted', events', Just newEvent)
            else let squashedEvent = simpleSquashEvent mPreviousEvent previousEvent newEvent
                     eventsToDeleted' = previousEvent : eventsToDeleted
                     events' = squashedEvent : events
                  in (eventsToDeleted', events', Just newEvent)
        Nothing ->
          let eventsToDeleted' = eventsToDeleted
              events' = newEvent : events
           in (eventsToDeleted', events', Just newEvent)
