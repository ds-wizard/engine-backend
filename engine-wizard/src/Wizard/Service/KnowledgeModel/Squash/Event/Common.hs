module Wizard.Service.KnowledgeModel.Squash.Event.Common where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Event.Event
import Shared.Model.Event.EventField
import Shared.Model.Event.EventLenses

class EventSquash oldEvent newEvent where
  squashEvent :: oldEvent -> newEvent -> [Event]

applyValue oldEvent newEvent accessor =
  case newEvent ^. accessor of
    (ChangedValue value) -> ChangedValue value
    NothingChanged -> oldEvent ^. accessor

class SimpleEventSquash event where
  isSimpleEventSquashApplicable :: event -> Bool
  isReorderEventSquashApplicable :: event -> event -> Bool
  isTypeChanged :: event -> event -> Bool
  simpleSquashEvent :: Maybe Event -> event -> event -> event

isChanged lens event =
  case event ^. lens of
    (ChangedValue _) -> True
    NothingChanged -> False

applyValueIfSameEntity mPreviousEvent oldEvent newEvent accessor =
  case mPreviousEvent of
    Just previousEvent ->
      if previousEvent ^. entityUuid' == newEvent ^. entityUuid
        then applyValue oldEvent newEvent accessor
        else NothingChanged
    _ -> NothingChanged
