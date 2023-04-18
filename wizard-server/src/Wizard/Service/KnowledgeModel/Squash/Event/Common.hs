module Wizard.Service.KnowledgeModel.Squash.Event.Common where

import WizardLib.KnowledgeModel.Model.Event.Event
import WizardLib.KnowledgeModel.Model.Event.EventField
import WizardLib.KnowledgeModel.Model.Event.EventLenses

class EventSquash oldEvent newEvent where
  squashEvent :: oldEvent -> newEvent -> [Event]

applyValue :: event -> event -> (event -> EventField value) -> EventField value
applyValue oldEvent newEvent getter =
  case getter newEvent of
    (ChangedValue value) -> ChangedValue value
    NothingChanged -> getter oldEvent

class SimpleEventSquash event where
  isSimpleEventSquashApplicable :: event -> Bool
  isReorderEventSquashApplicable :: event -> event -> Bool
  isTypeChanged :: event -> event -> Bool
  simpleSquashEvent :: Maybe Event -> event -> event -> event

isChanged getter event =
  case getter event of
    (ChangedValue _) -> True
    NothingChanged -> False

applyValueIfSameEntity mPreviousEvent oldEvent newEvent accessor =
  case mPreviousEvent of
    Just previousEvent ->
      if getEntityUuid previousEvent == newEvent.entityUuid
        then applyValue oldEvent newEvent accessor
        else NothingChanged
    _ -> NothingChanged
