module Wizard.Service.KnowledgeModel.Squash.Event.Question where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Model.Common.MapEntry
import Shared.Model.Event.EventField
import Shared.Model.Event.EventLenses
import Shared.Model.Event.Question.QuestionEvent
import Shared.Model.Event.Question.QuestionEventLenses ()
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditQuestionEvent where
  isSimpleEventSquashApplicable (EditOptionsQuestionEvent' event) =
    not $
      isChanged (requiredPhaseUuid :: EditOptionsQuestionEvent -> EventField (Maybe U.UUID)) event
        || isChanged (tagUuids :: EditOptionsQuestionEvent -> EventField [U.UUID]) event
        || isChanged (expertUuids :: EditOptionsQuestionEvent -> EventField [U.UUID]) event
        || isChanged (referenceUuids :: EditOptionsQuestionEvent -> EventField [U.UUID]) event
        || isChanged (answerUuids :: EditOptionsQuestionEvent -> EventField [U.UUID]) event
  isSimpleEventSquashApplicable (EditMultiChoiceQuestionEvent' event) =
    not $
      isChanged (requiredPhaseUuid :: EditMultiChoiceQuestionEvent -> EventField (Maybe U.UUID)) event
        || isChanged (tagUuids :: EditMultiChoiceQuestionEvent -> EventField [U.UUID]) event
        || isChanged (expertUuids :: EditMultiChoiceQuestionEvent -> EventField [U.UUID]) event
        || isChanged (referenceUuids :: EditMultiChoiceQuestionEvent -> EventField [U.UUID]) event
        || isChanged (choiceUuids :: EditMultiChoiceQuestionEvent -> EventField [U.UUID]) event
  isSimpleEventSquashApplicable (EditListQuestionEvent' event) =
    not $
      isChanged (requiredPhaseUuid :: EditListQuestionEvent -> EventField (Maybe U.UUID)) event
        || isChanged (tagUuids :: EditListQuestionEvent -> EventField [U.UUID]) event
        || isChanged (expertUuids :: EditListQuestionEvent -> EventField [U.UUID]) event
        || isChanged (referenceUuids :: EditListQuestionEvent -> EventField [U.UUID]) event
        || isChanged (itemTemplateQuestionUuids :: EditListQuestionEvent -> EventField [U.UUID]) event
  isSimpleEventSquashApplicable (EditValueQuestionEvent' event) =
    not $
      isChanged (requiredPhaseUuid :: EditValueQuestionEvent -> EventField (Maybe U.UUID)) event
        || isChanged (tagUuids :: EditValueQuestionEvent -> EventField [U.UUID]) event
        || isChanged (expertUuids :: EditValueQuestionEvent -> EventField [U.UUID]) event
        || isChanged (referenceUuids :: EditValueQuestionEvent -> EventField [U.UUID]) event
  isSimpleEventSquashApplicable (EditIntegrationQuestionEvent' event) =
    not $
      isChanged (requiredPhaseUuid :: EditIntegrationQuestionEvent -> EventField (Maybe U.UUID)) event
        || isChanged (tagUuids :: EditIntegrationQuestionEvent -> EventField [U.UUID]) event
        || isChanged (expertUuids :: EditIntegrationQuestionEvent -> EventField [U.UUID]) event
        || isChanged (referenceUuids :: EditIntegrationQuestionEvent -> EventField [U.UUID]) event
        || isChanged (integrationUuid :: EditIntegrationQuestionEvent -> EventField U.UUID) event

  --  --------------------------------------
  isReorderEventSquashApplicable previousEvent newEvent = getEntityUuid previousEvent == getEntityUuid newEvent

  --  --------------------------------------
  isTypeChanged (EditOptionsQuestionEvent' oldEvent) (EditOptionsQuestionEvent' newEvent) = False
  isTypeChanged (EditMultiChoiceQuestionEvent' oldEvent) (EditMultiChoiceQuestionEvent' newEvent) = False
  isTypeChanged (EditListQuestionEvent' oldEvent) (EditListQuestionEvent' newEvent) = False
  isTypeChanged (EditValueQuestionEvent' oldEvent) (EditValueQuestionEvent' newEvent) = False
  isTypeChanged (EditIntegrationQuestionEvent' oldEvent) (EditIntegrationQuestionEvent' newEvent) = False
  isTypeChanged _ _ = True

  --  --------------------------------------
  simpleSquashEvent mPreviousEvent (EditOptionsQuestionEvent' oldEvent) (EditOptionsQuestionEvent' newEvent) =
    EditOptionsQuestionEvent' $
      EditOptionsQuestionEvent
        { uuid = newEvent.uuid
        , parentUuid = newEvent.parentUuid
        , entityUuid = newEvent.entityUuid
        , title = applyValue oldEvent newEvent (title :: EditOptionsQuestionEvent -> EventField String)
        , text = applyValue oldEvent newEvent (text :: EditOptionsQuestionEvent -> EventField (Maybe String))
        , requiredPhaseUuid = applyValue oldEvent newEvent (requiredPhaseUuid :: EditOptionsQuestionEvent -> EventField (Maybe U.UUID))
        , annotations = applyValue oldEvent newEvent (annotations :: EditOptionsQuestionEvent -> EventField [MapEntry String String])
        , tagUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (tagUuids :: EditOptionsQuestionEvent -> EventField [U.UUID])
        , expertUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (expertUuids :: EditOptionsQuestionEvent -> EventField [U.UUID])
        , referenceUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (referenceUuids :: EditOptionsQuestionEvent -> EventField [U.UUID])
        , answerUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (answerUuids :: EditOptionsQuestionEvent -> EventField [U.UUID])
        , createdAt = newEvent.createdAt
        }
  simpleSquashEvent mPreviousEvent (EditMultiChoiceQuestionEvent' oldEvent) (EditMultiChoiceQuestionEvent' newEvent) =
    EditMultiChoiceQuestionEvent' $
      EditMultiChoiceQuestionEvent
        { uuid = newEvent.uuid
        , parentUuid = newEvent.parentUuid
        , entityUuid = newEvent.entityUuid
        , title = applyValue oldEvent newEvent (title :: EditMultiChoiceQuestionEvent -> EventField String)
        , text = applyValue oldEvent newEvent (text :: EditMultiChoiceQuestionEvent -> EventField (Maybe String))
        , requiredPhaseUuid = applyValue oldEvent newEvent (requiredPhaseUuid :: EditMultiChoiceQuestionEvent -> EventField (Maybe U.UUID))
        , annotations = applyValue oldEvent newEvent (annotations :: EditMultiChoiceQuestionEvent -> EventField [MapEntry String String])
        , tagUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (tagUuids :: EditMultiChoiceQuestionEvent -> EventField [U.UUID])
        , expertUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (expertUuids :: EditMultiChoiceQuestionEvent -> EventField [U.UUID])
        , referenceUuids =
            applyValueIfSameEntity mPreviousEvent oldEvent newEvent (referenceUuids :: EditMultiChoiceQuestionEvent -> EventField [U.UUID])
        , choiceUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (choiceUuids :: EditMultiChoiceQuestionEvent -> EventField [U.UUID])
        , createdAt = newEvent.createdAt
        }
  simpleSquashEvent mPreviousEvent (EditListQuestionEvent' oldEvent) (EditListQuestionEvent' newEvent) =
    EditListQuestionEvent' $
      EditListQuestionEvent
        { uuid = newEvent.uuid
        , parentUuid = newEvent.parentUuid
        , entityUuid = newEvent.entityUuid
        , title = applyValue oldEvent newEvent (title :: EditListQuestionEvent -> EventField String)
        , text = applyValue oldEvent newEvent (text :: EditListQuestionEvent -> EventField (Maybe String))
        , requiredPhaseUuid = applyValue oldEvent newEvent (requiredPhaseUuid :: EditListQuestionEvent -> EventField (Maybe U.UUID))
        , annotations = applyValue oldEvent newEvent (annotations :: EditListQuestionEvent -> EventField [MapEntry String String])
        , tagUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (tagUuids :: EditListQuestionEvent -> EventField [U.UUID])
        , expertUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (expertUuids :: EditListQuestionEvent -> EventField [U.UUID])
        , referenceUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (referenceUuids :: EditListQuestionEvent -> EventField [U.UUID])
        , itemTemplateQuestionUuids =
            applyValueIfSameEntity mPreviousEvent oldEvent newEvent (itemTemplateQuestionUuids :: EditListQuestionEvent -> EventField [U.UUID])
        , createdAt = newEvent.createdAt
        }
  simpleSquashEvent mPreviousEvent (EditValueQuestionEvent' oldEvent) (EditValueQuestionEvent' newEvent) =
    EditValueQuestionEvent' $
      EditValueQuestionEvent
        { uuid = newEvent.uuid
        , parentUuid = newEvent.parentUuid
        , entityUuid = newEvent.entityUuid
        , title = applyValue oldEvent newEvent (title :: EditValueQuestionEvent -> EventField String)
        , text = applyValue oldEvent newEvent (text :: EditValueQuestionEvent -> EventField (Maybe String))
        , requiredPhaseUuid = applyValue oldEvent newEvent (requiredPhaseUuid :: EditValueQuestionEvent -> EventField (Maybe U.UUID))
        , annotations = applyValue oldEvent newEvent (annotations :: EditValueQuestionEvent -> EventField [MapEntry String String])
        , tagUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (tagUuids :: EditValueQuestionEvent -> EventField [U.UUID])
        , expertUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (expertUuids :: EditValueQuestionEvent -> EventField [U.UUID])
        , referenceUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (referenceUuids :: EditValueQuestionEvent -> EventField [U.UUID])
        , valueType = applyValue oldEvent newEvent (valueType :: EditValueQuestionEvent -> EventField QuestionValueType)
        , createdAt = newEvent.createdAt
        }
  simpleSquashEvent mPreviousEvent (EditIntegrationQuestionEvent' oldEvent) (EditIntegrationQuestionEvent' newEvent) =
    EditIntegrationQuestionEvent' $
      EditIntegrationQuestionEvent
        { uuid = newEvent.uuid
        , parentUuid = newEvent.parentUuid
        , entityUuid = newEvent.entityUuid
        , title = applyValue oldEvent newEvent (title :: EditIntegrationQuestionEvent -> EventField String)
        , text = applyValue oldEvent newEvent (text :: EditIntegrationQuestionEvent -> EventField (Maybe String))
        , requiredPhaseUuid = applyValue oldEvent newEvent (requiredPhaseUuid :: EditIntegrationQuestionEvent -> EventField (Maybe U.UUID))
        , annotations = applyValue oldEvent newEvent (annotations :: EditIntegrationQuestionEvent -> EventField [MapEntry String String])
        , tagUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (tagUuids :: EditIntegrationQuestionEvent -> EventField [U.UUID])
        , expertUuids = applyValueIfSameEntity mPreviousEvent oldEvent newEvent (expertUuids :: EditIntegrationQuestionEvent -> EventField [U.UUID])
        , referenceUuids =
            applyValueIfSameEntity mPreviousEvent oldEvent newEvent (referenceUuids :: EditIntegrationQuestionEvent -> EventField [U.UUID])
        , integrationUuid =
            applyValueIfSameEntity mPreviousEvent oldEvent newEvent (integrationUuid :: EditIntegrationQuestionEvent -> EventField U.UUID)
        , props = applyValue oldEvent newEvent (props :: EditIntegrationQuestionEvent -> EventField (M.Map String String))
        , createdAt = newEvent.createdAt
        }
