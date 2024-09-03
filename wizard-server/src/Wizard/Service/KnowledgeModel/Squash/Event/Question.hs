module Wizard.Service.KnowledgeModel.Squash.Event.Question where

import Wizard.Service.KnowledgeModel.Squash.Event.Common
import WizardLib.KnowledgeModel.Model.Event.EventLenses
import WizardLib.KnowledgeModel.Model.Event.Question.QuestionEvent

instance SimpleEventSquash EditQuestionEvent where
  isSimpleEventSquashApplicable (EditOptionsQuestionEvent' event) =
    not $
      isChanged (.requiredPhaseUuid) event
        || isChanged (.tagUuids) event
        || isChanged (.expertUuids) event
        || isChanged (.referenceUuids) event
        || isChanged (.answerUuids) event
  isSimpleEventSquashApplicable (EditMultiChoiceQuestionEvent' event) =
    not $
      isChanged (.requiredPhaseUuid) event
        || isChanged (.tagUuids) event
        || isChanged (.expertUuids) event
        || isChanged (.referenceUuids) event
        || isChanged (.choiceUuids) event
  isSimpleEventSquashApplicable (EditListQuestionEvent' event) =
    not $
      isChanged (.requiredPhaseUuid) event
        || isChanged (.tagUuids) event
        || isChanged (.expertUuids) event
        || isChanged (.referenceUuids) event
        || isChanged (.itemTemplateQuestionUuids) event
  isSimpleEventSquashApplicable (EditValueQuestionEvent' event) =
    not $
      isChanged (.requiredPhaseUuid) event
        || isChanged (.tagUuids) event
        || isChanged (.expertUuids) event
        || isChanged (.referenceUuids) event
  isSimpleEventSquashApplicable (EditIntegrationQuestionEvent' event) =
    not $
      isChanged (.requiredPhaseUuid) event
        || isChanged (.tagUuids) event
        || isChanged (.expertUuids) event
        || isChanged (.referenceUuids) event
        || isChanged (.integrationUuid) event
  isSimpleEventSquashApplicable (EditItemSelectQuestionEvent' event) =
    not $
      isChanged (.requiredPhaseUuid) event
        || isChanged (.tagUuids) event
        || isChanged (.expertUuids) event
        || isChanged (.referenceUuids) event
        || isChanged (.listQuestionUuid) event

  --  --------------------------------------
  isReorderEventSquashApplicable previousEvent newEvent = getEntityUuid previousEvent == getEntityUuid newEvent

  --  --------------------------------------
  isTypeChanged (EditOptionsQuestionEvent' oldEvent) (EditOptionsQuestionEvent' newEvent) = False
  isTypeChanged (EditMultiChoiceQuestionEvent' oldEvent) (EditMultiChoiceQuestionEvent' newEvent) = False
  isTypeChanged (EditListQuestionEvent' oldEvent) (EditListQuestionEvent' newEvent) = False
  isTypeChanged (EditValueQuestionEvent' oldEvent) (EditValueQuestionEvent' newEvent) = False
  isTypeChanged (EditIntegrationQuestionEvent' oldEvent) (EditIntegrationQuestionEvent' newEvent) = False
  isTypeChanged (EditItemSelectQuestionEvent' oldEvent) (EditItemSelectQuestionEvent' newEvent) = False
  isTypeChanged _ _ = True

  --  --------------------------------------
  simpleSquashEvent mPreviousEvent (EditOptionsQuestionEvent' oldEvent) (EditOptionsQuestionEvent' newEvent) =
    EditOptionsQuestionEvent' $
      EditOptionsQuestionEvent
        { uuid = newEvent.uuid
        , parentUuid = newEvent.parentUuid
        , entityUuid = newEvent.entityUuid
        , title = applyValue oldEvent newEvent (.title)
        , text = applyValue oldEvent newEvent (.text)
        , requiredPhaseUuid = applyValue oldEvent newEvent (.requiredPhaseUuid)
        , annotations = applyValue oldEvent newEvent (.annotations)
        , tagUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.tagUuids)
        , expertUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.expertUuids)
        , referenceUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.referenceUuids)
        , answerUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.answerUuids)
        , createdAt = newEvent.createdAt
        }
  simpleSquashEvent mPreviousEvent (EditMultiChoiceQuestionEvent' oldEvent) (EditMultiChoiceQuestionEvent' newEvent) =
    EditMultiChoiceQuestionEvent' $
      EditMultiChoiceQuestionEvent
        { uuid = newEvent.uuid
        , parentUuid = newEvent.parentUuid
        , entityUuid = newEvent.entityUuid
        , title = applyValue oldEvent newEvent (.title)
        , text = applyValue oldEvent newEvent (.text)
        , requiredPhaseUuid = applyValue oldEvent newEvent (.requiredPhaseUuid)
        , annotations = applyValue oldEvent newEvent (.annotations)
        , tagUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.tagUuids)
        , expertUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.expertUuids)
        , referenceUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.referenceUuids)
        , choiceUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.choiceUuids)
        , createdAt = newEvent.createdAt
        }
  simpleSquashEvent mPreviousEvent (EditListQuestionEvent' oldEvent) (EditListQuestionEvent' newEvent) =
    EditListQuestionEvent' $
      EditListQuestionEvent
        { uuid = newEvent.uuid
        , parentUuid = newEvent.parentUuid
        , entityUuid = newEvent.entityUuid
        , title = applyValue oldEvent newEvent (.title)
        , text = applyValue oldEvent newEvent (.text)
        , requiredPhaseUuid = applyValue oldEvent newEvent (.requiredPhaseUuid)
        , annotations = applyValue oldEvent newEvent (.annotations)
        , tagUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.tagUuids)
        , expertUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.expertUuids)
        , referenceUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.referenceUuids)
        , itemTemplateQuestionUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.itemTemplateQuestionUuids)
        , createdAt = newEvent.createdAt
        }
  simpleSquashEvent mPreviousEvent (EditValueQuestionEvent' oldEvent) (EditValueQuestionEvent' newEvent) =
    EditValueQuestionEvent' $
      EditValueQuestionEvent
        { uuid = newEvent.uuid
        , parentUuid = newEvent.parentUuid
        , entityUuid = newEvent.entityUuid
        , title = applyValue oldEvent newEvent (.title)
        , text = applyValue oldEvent newEvent (.text)
        , requiredPhaseUuid = applyValue oldEvent newEvent (.requiredPhaseUuid)
        , annotations = applyValue oldEvent newEvent (.annotations)
        , tagUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.tagUuids)
        , expertUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.expertUuids)
        , referenceUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.referenceUuids)
        , valueType = applyValue oldEvent newEvent (.valueType)
        , createdAt = newEvent.createdAt
        }
  simpleSquashEvent mPreviousEvent (EditIntegrationQuestionEvent' oldEvent) (EditIntegrationQuestionEvent' newEvent) =
    EditIntegrationQuestionEvent' $
      EditIntegrationQuestionEvent
        { uuid = newEvent.uuid
        , parentUuid = newEvent.parentUuid
        , entityUuid = newEvent.entityUuid
        , title = applyValue oldEvent newEvent (.title)
        , text = applyValue oldEvent newEvent (.text)
        , requiredPhaseUuid = applyValue oldEvent newEvent (.requiredPhaseUuid)
        , annotations = applyValue oldEvent newEvent (.annotations)
        , tagUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.tagUuids)
        , expertUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.expertUuids)
        , referenceUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.referenceUuids)
        , integrationUuid = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.integrationUuid)
        , props = applyValue oldEvent newEvent (.props)
        , createdAt = newEvent.createdAt
        }
  simpleSquashEvent mPreviousEvent (EditItemSelectQuestionEvent' oldEvent) (EditItemSelectQuestionEvent' newEvent) =
    EditItemSelectQuestionEvent' $
      EditItemSelectQuestionEvent
        { uuid = newEvent.uuid
        , parentUuid = newEvent.parentUuid
        , entityUuid = newEvent.entityUuid
        , title = applyValue oldEvent newEvent (.title)
        , text = applyValue oldEvent newEvent (.text)
        , requiredPhaseUuid = applyValue oldEvent newEvent (.requiredPhaseUuid)
        , annotations = applyValue oldEvent newEvent (.annotations)
        , tagUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.tagUuids)
        , expertUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.expertUuids)
        , referenceUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.referenceUuids)
        , listQuestionUuid = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (.listQuestionUuid)
        , createdAt = newEvent.createdAt
        }
