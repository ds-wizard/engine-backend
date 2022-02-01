module Wizard.Service.KnowledgeModel.Squash.Event.Common where

import Control.Lens ((^.))

import Shared.Model.Event.Event
import Shared.Model.Event.EventField

class EventSquash oldEvent newEvent where
  squashEvent :: oldEvent -> newEvent -> [Event]

applyValue oldEvent newEvent lens =
  case newEvent ^. lens of
    (ChangedValue value) -> ChangedValue value
    NothingChanged -> oldEvent ^. lens

class SimpleEventSquash event where
  isSimpleEventSquashApplicable :: event -> Bool
  isTypeChanged :: event -> event -> Bool
  simpleSquashEvent :: event -> event -> event

isChanged lens event =
  case event ^. lens of
    (ChangedValue _) -> True
    NothingChanged -> False
