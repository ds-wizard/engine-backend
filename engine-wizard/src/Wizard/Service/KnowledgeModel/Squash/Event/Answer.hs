module Wizard.Service.KnowledgeModel.Squash.Event.Answer where

import Shared.Model.Event.Answer.AnswerEvent
import Shared.Model.Event.EventLenses
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditAnswerEvent where
  isSimpleEventSquashApplicable = not . isChanged (.followUpUuids)
  isReorderEventSquashApplicable previousEvent newEvent = getEntityUuid previousEvent == getEntityUuid newEvent
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent oldEvent newEvent =
    EditAnswerEvent
      { uuid = newEvent.uuid
      , parentUuid = newEvent.parentUuid
      , entityUuid = newEvent.entityUuid
      , aLabel = applyValue oldEvent newEvent (.aLabel)
      , advice = applyValue oldEvent newEvent (.advice)
      , annotations = applyValue oldEvent newEvent (.annotations)
      , followUpUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.followUpUuids)
      , metricMeasures = applyValue oldEvent newEvent (.metricMeasures)
      , createdAt = newEvent.createdAt
      }
