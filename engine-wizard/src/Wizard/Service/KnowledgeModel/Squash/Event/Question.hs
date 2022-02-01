module Wizard.Service.KnowledgeModel.Squash.Event.Question where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Event.EventField
import Shared.Model.Event.Question.QuestionEvent
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditQuestionEvent where
  isSimpleEventSquashApplicable (EditOptionsQuestionEvent' event) =
    not $
    isChanged requiredPhaseUuid event ||
    isChanged tagUuids event ||
    isChanged expertUuids event || isChanged referenceUuids event || isChanged answerUuids event
  isSimpleEventSquashApplicable (EditMultiChoiceQuestionEvent' event) =
    not $
    isChanged requiredPhaseUuid event ||
    isChanged tagUuids event ||
    isChanged expertUuids event || isChanged referenceUuids event || isChanged choiceUuids event
  isSimpleEventSquashApplicable (EditListQuestionEvent' event) =
    not $
    isChanged requiredPhaseUuid event ||
    isChanged tagUuids event ||
    isChanged expertUuids event || isChanged referenceUuids event || isChanged itemTemplateQuestionUuids event
  isSimpleEventSquashApplicable (EditValueQuestionEvent' event) =
    not $
    isChanged requiredPhaseUuid event ||
    isChanged tagUuids event || isChanged expertUuids event || isChanged referenceUuids event
  isSimpleEventSquashApplicable (EditIntegrationQuestionEvent' event) =
    not $
    isChanged requiredPhaseUuid event ||
    isChanged tagUuids event ||
    isChanged expertUuids event || isChanged referenceUuids event || isChanged integrationUuid event
  --  --------------------------------------
  isTypeChanged (EditOptionsQuestionEvent' oldEvent) (EditOptionsQuestionEvent' newEvent) = False
  isTypeChanged (EditMultiChoiceQuestionEvent' oldEvent) (EditMultiChoiceQuestionEvent' newEvent) = False
  isTypeChanged (EditListQuestionEvent' oldEvent) (EditListQuestionEvent' newEvent) = False
  isTypeChanged (EditValueQuestionEvent' oldEvent) (EditValueQuestionEvent' newEvent) = False
  isTypeChanged (EditIntegrationQuestionEvent' oldEvent) (EditIntegrationQuestionEvent' newEvent) = False
  isTypeChanged _ _ = True
  --  --------------------------------------
  simpleSquashEvent (EditOptionsQuestionEvent' oldEvent) (EditOptionsQuestionEvent' newEvent) =
    EditOptionsQuestionEvent' $
    EditOptionsQuestionEvent
      { _editOptionsQuestionEventUuid = newEvent ^. uuid
      , _editOptionsQuestionEventParentUuid = newEvent ^. parentUuid
      , _editOptionsQuestionEventEntityUuid = newEvent ^. entityUuid
      , _editOptionsQuestionEventTitle = applyValue oldEvent newEvent title
      , _editOptionsQuestionEventText = applyValue oldEvent newEvent text
      , _editOptionsQuestionEventRequiredPhaseUuid = applyValue oldEvent newEvent requiredPhaseUuid
      , _editOptionsQuestionEventAnnotations = applyValue oldEvent newEvent annotations
      , _editOptionsQuestionEventTagUuids = NothingChanged
      , _editOptionsQuestionEventExpertUuids = NothingChanged
      , _editOptionsQuestionEventReferenceUuids = NothingChanged
      , _editOptionsQuestionEventAnswerUuids = NothingChanged
      , _editOptionsQuestionEventCreatedAt = newEvent ^. createdAt
      }
  simpleSquashEvent (EditMultiChoiceQuestionEvent' oldEvent) (EditMultiChoiceQuestionEvent' newEvent) =
    EditMultiChoiceQuestionEvent' $
    EditMultiChoiceQuestionEvent
      { _editMultiChoiceQuestionEventUuid = newEvent ^. uuid
      , _editMultiChoiceQuestionEventParentUuid = newEvent ^. parentUuid
      , _editMultiChoiceQuestionEventEntityUuid = newEvent ^. entityUuid
      , _editMultiChoiceQuestionEventTitle = applyValue oldEvent newEvent title
      , _editMultiChoiceQuestionEventText = applyValue oldEvent newEvent text
      , _editMultiChoiceQuestionEventRequiredPhaseUuid = applyValue oldEvent newEvent requiredPhaseUuid
      , _editMultiChoiceQuestionEventAnnotations = applyValue oldEvent newEvent annotations
      , _editMultiChoiceQuestionEventTagUuids = NothingChanged
      , _editMultiChoiceQuestionEventExpertUuids = NothingChanged
      , _editMultiChoiceQuestionEventReferenceUuids = NothingChanged
      , _editMultiChoiceQuestionEventChoiceUuids = NothingChanged
      , _editMultiChoiceQuestionEventCreatedAt = newEvent ^. createdAt
      }
  simpleSquashEvent (EditListQuestionEvent' oldEvent) (EditListQuestionEvent' newEvent) =
    EditListQuestionEvent' $
    EditListQuestionEvent
      { _editListQuestionEventUuid = newEvent ^. uuid
      , _editListQuestionEventParentUuid = newEvent ^. parentUuid
      , _editListQuestionEventEntityUuid = newEvent ^. entityUuid
      , _editListQuestionEventTitle = applyValue oldEvent newEvent title
      , _editListQuestionEventText = applyValue oldEvent newEvent text
      , _editListQuestionEventRequiredPhaseUuid = applyValue oldEvent newEvent requiredPhaseUuid
      , _editListQuestionEventAnnotations = applyValue oldEvent newEvent annotations
      , _editListQuestionEventTagUuids = NothingChanged
      , _editListQuestionEventExpertUuids = NothingChanged
      , _editListQuestionEventReferenceUuids = NothingChanged
      , _editListQuestionEventItemTemplateQuestionUuids = NothingChanged
      , _editListQuestionEventCreatedAt = newEvent ^. createdAt
      }
  simpleSquashEvent (EditValueQuestionEvent' oldEvent) (EditValueQuestionEvent' newEvent) =
    EditValueQuestionEvent' $
    EditValueQuestionEvent
      { _editValueQuestionEventUuid = newEvent ^. uuid
      , _editValueQuestionEventParentUuid = newEvent ^. parentUuid
      , _editValueQuestionEventEntityUuid = newEvent ^. entityUuid
      , _editValueQuestionEventTitle = applyValue oldEvent newEvent title
      , _editValueQuestionEventText = applyValue oldEvent newEvent text
      , _editValueQuestionEventRequiredPhaseUuid = applyValue oldEvent newEvent requiredPhaseUuid
      , _editValueQuestionEventAnnotations = applyValue oldEvent newEvent annotations
      , _editValueQuestionEventTagUuids = NothingChanged
      , _editValueQuestionEventExpertUuids = NothingChanged
      , _editValueQuestionEventReferenceUuids = NothingChanged
      , _editValueQuestionEventValueType = applyValue oldEvent newEvent valueType
      , _editValueQuestionEventCreatedAt = newEvent ^. createdAt
      }
  simpleSquashEvent (EditIntegrationQuestionEvent' oldEvent) (EditIntegrationQuestionEvent' newEvent) =
    EditIntegrationQuestionEvent' $
    EditIntegrationQuestionEvent
      { _editIntegrationQuestionEventUuid = newEvent ^. uuid
      , _editIntegrationQuestionEventParentUuid = newEvent ^. parentUuid
      , _editIntegrationQuestionEventEntityUuid = newEvent ^. entityUuid
      , _editIntegrationQuestionEventTitle = applyValue oldEvent newEvent title
      , _editIntegrationQuestionEventText = applyValue oldEvent newEvent text
      , _editIntegrationQuestionEventRequiredPhaseUuid = applyValue oldEvent newEvent requiredPhaseUuid
      , _editIntegrationQuestionEventAnnotations = applyValue oldEvent newEvent annotations
      , _editIntegrationQuestionEventTagUuids = NothingChanged
      , _editIntegrationQuestionEventExpertUuids = NothingChanged
      , _editIntegrationQuestionEventReferenceUuids = NothingChanged
      , _editIntegrationQuestionEventIntegrationUuid = NothingChanged
      , _editIntegrationQuestionEventProps = applyValue oldEvent newEvent props
      , _editIntegrationQuestionEventCreatedAt = newEvent ^. createdAt
      }
