module Wizard.Service.KnowledgeModel.Squash.Event.Answer where

import Wizard.Service.KnowledgeModel.Squash.Event.Common
import WizardLib.KnowledgeModel.Model.Event.Answer.AnswerEvent
import WizardLib.KnowledgeModel.Model.Event.EventLenses

instance SimpleEventSquash EditAnswerEvent where
  isSimpleEventSquashApplicable e = not (isChanged (.followUpUuids) e || isChanged (.metricMeasures) e)
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
      , createdAt = oldEvent.createdAt
      }
