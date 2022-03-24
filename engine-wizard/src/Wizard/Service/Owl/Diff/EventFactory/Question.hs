module Wizard.Service.Owl.Diff.EventFactory.Question where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Time

import LensesConfig
import Shared.Model.Common.Lens
import Shared.Model.Event.Event
import Shared.Model.Event.EventField
import Shared.Model.Event.EventUtil
import Shared.Model.Event.Question.QuestionEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Shared.Util.Uuid
import Wizard.Service.Owl.Diff.Accessor.Accessor
import Wizard.Service.Owl.Diff.EventFactory.EventFactory

instance EventFactory Question where
  createAddEvent parentUuid (OptionsQuestion' entity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      AddQuestionEvent' $
      AddOptionsQuestionEvent' $
      AddOptionsQuestionEvent
        { _addOptionsQuestionEventUuid = eventUuid
        , _addOptionsQuestionEventParentUuid = parentUuid
        , _addOptionsQuestionEventEntityUuid = entity ^. uuid
        , _addOptionsQuestionEventTitle = entity ^. title
        , _addOptionsQuestionEventText = entity ^. text
        , _addOptionsQuestionEventRequiredPhaseUuid = entity ^. requiredPhaseUuid
        , _addOptionsQuestionEventAnnotations = entity ^. annotations
        , _addOptionsQuestionEventTagUuids = entity ^. tagUuids
        , _addOptionsQuestionEventCreatedAt = now
        }
  createAddEvent parentUuid (MultiChoiceQuestion' entity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      AddQuestionEvent' $
      AddMultiChoiceQuestionEvent' $
      AddMultiChoiceQuestionEvent
        { _addMultiChoiceQuestionEventUuid = eventUuid
        , _addMultiChoiceQuestionEventParentUuid = parentUuid
        , _addMultiChoiceQuestionEventEntityUuid = entity ^. uuid
        , _addMultiChoiceQuestionEventTitle = entity ^. title
        , _addMultiChoiceQuestionEventText = entity ^. text
        , _addMultiChoiceQuestionEventRequiredPhaseUuid = entity ^. requiredPhaseUuid
        , _addMultiChoiceQuestionEventAnnotations = entity ^. annotations
        , _addMultiChoiceQuestionEventTagUuids = entity ^. tagUuids
        , _addMultiChoiceQuestionEventCreatedAt = now
        }
  createAddEvent parentUuid (ListQuestion' entity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      AddQuestionEvent' $
      AddListQuestionEvent' $
      AddListQuestionEvent
        { _addListQuestionEventUuid = eventUuid
        , _addListQuestionEventParentUuid = parentUuid
        , _addListQuestionEventEntityUuid = entity ^. uuid
        , _addListQuestionEventTitle = entity ^. title
        , _addListQuestionEventText = entity ^. text
        , _addListQuestionEventRequiredPhaseUuid = entity ^. requiredPhaseUuid
        , _addListQuestionEventAnnotations = entity ^. annotations
        , _addListQuestionEventTagUuids = entity ^. tagUuids
        , _addListQuestionEventCreatedAt = now
        }
  createAddEvent parentUuid (ValueQuestion' entity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      AddQuestionEvent' $
      AddValueQuestionEvent' $
      AddValueQuestionEvent
        { _addValueQuestionEventUuid = eventUuid
        , _addValueQuestionEventParentUuid = parentUuid
        , _addValueQuestionEventEntityUuid = entity ^. uuid
        , _addValueQuestionEventTitle = entity ^. title
        , _addValueQuestionEventText = entity ^. text
        , _addValueQuestionEventRequiredPhaseUuid = entity ^. requiredPhaseUuid
        , _addValueQuestionEventAnnotations = entity ^. annotations
        , _addValueQuestionEventTagUuids = entity ^. tagUuids
        , _addValueQuestionEventValueType = entity ^. valueType
        , _addValueQuestionEventCreatedAt = now
        }
  createAddEvent parentUuid (IntegrationQuestion' entity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      AddQuestionEvent' $
      AddIntegrationQuestionEvent' $
      AddIntegrationQuestionEvent
        { _addIntegrationQuestionEventUuid = eventUuid
        , _addIntegrationQuestionEventParentUuid = parentUuid
        , _addIntegrationQuestionEventEntityUuid = entity ^. uuid
        , _addIntegrationQuestionEventTitle = entity ^. title
        , _addIntegrationQuestionEventText = entity ^. text
        , _addIntegrationQuestionEventRequiredPhaseUuid = entity ^. requiredPhaseUuid
        , _addIntegrationQuestionEventAnnotations = entity ^. annotations
        , _addIntegrationQuestionEventTagUuids = entity ^. tagUuids
        , _addIntegrationQuestionEventIntegrationUuid = entity ^. integrationUuid
        , _addIntegrationQuestionEventProps = entity ^. props
        , _addIntegrationQuestionEventCreatedAt = now
        }
  createEditEvent (oldKm, newKm) parentUuid (OptionsQuestion' oldEntity) (OptionsQuestion' newEntity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let event =
          EditOptionsQuestionEvent
            { _editOptionsQuestionEventUuid = eventUuid
            , _editOptionsQuestionEventParentUuid = parentUuid
            , _editOptionsQuestionEventEntityUuid = newEntity ^. uuid
            , _editOptionsQuestionEventTitle = diffField (oldEntity ^. title) (newEntity ^. title)
            , _editOptionsQuestionEventText = diffField (oldEntity ^. text) (newEntity ^. text)
            , _editOptionsQuestionEventRequiredPhaseUuid =
                diffField (oldEntity ^. requiredPhaseUuid) (newEntity ^. requiredPhaseUuid)
            , _editOptionsQuestionEventAnnotations = diffField (oldEntity ^. annotations) (newEntity ^. annotations)
            , _editOptionsQuestionEventTagUuids = diffField (oldEntity ^. tagUuids) (newEntity ^. tagUuids)
            , _editOptionsQuestionEventExpertUuids = diffField (oldEntity ^. expertUuids) (newEntity ^. expertUuids)
            , _editOptionsQuestionEventReferenceUuids =
                diffField (oldEntity ^. referenceUuids) (newEntity ^. referenceUuids)
            , _editOptionsQuestionEventAnswerUuids =
                diffListField (oldKm, newKm) (oldEntity ^. answerUuids) (newEntity ^. answerUuids) answersM
            , _editOptionsQuestionEventCreatedAt = now
            }
    if isEmptyEvent event
      then return . Just . EditQuestionEvent' . EditOptionsQuestionEvent' $ event
      else return Nothing
  createEditEvent (oldKm, newKm) parentUuid (MultiChoiceQuestion' oldEntity) (MultiChoiceQuestion' newEntity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let event =
          EditMultiChoiceQuestionEvent
            { _editMultiChoiceQuestionEventUuid = eventUuid
            , _editMultiChoiceQuestionEventParentUuid = parentUuid
            , _editMultiChoiceQuestionEventEntityUuid = newEntity ^. uuid
            , _editMultiChoiceQuestionEventTitle = diffField (oldEntity ^. title) (newEntity ^. title)
            , _editMultiChoiceQuestionEventText = diffField (oldEntity ^. text) (newEntity ^. text)
            , _editMultiChoiceQuestionEventRequiredPhaseUuid =
                diffField (oldEntity ^. requiredPhaseUuid) (newEntity ^. requiredPhaseUuid)
            , _editMultiChoiceQuestionEventAnnotations = diffField (oldEntity ^. annotations) (newEntity ^. annotations)
            , _editMultiChoiceQuestionEventTagUuids = diffField (oldEntity ^. tagUuids) (newEntity ^. tagUuids)
            , _editMultiChoiceQuestionEventExpertUuids = diffField (oldEntity ^. expertUuids) (newEntity ^. expertUuids)
            , _editMultiChoiceQuestionEventReferenceUuids =
                diffField (oldEntity ^. referenceUuids) (newEntity ^. referenceUuids)
            , _editMultiChoiceQuestionEventChoiceUuids =
                diffListField (oldKm, newKm) (oldEntity ^. choiceUuids) (newEntity ^. choiceUuids) choicesM
            , _editMultiChoiceQuestionEventCreatedAt = now
            }
    if isEmptyEvent event
      then return . Just . EditQuestionEvent' . EditMultiChoiceQuestionEvent' $ event
      else return Nothing
  createEditEvent (oldKm, newKm) parentUuid (ListQuestion' oldEntity) (ListQuestion' newEntity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let event =
          EditListQuestionEvent
            { _editListQuestionEventUuid = eventUuid
            , _editListQuestionEventParentUuid = parentUuid
            , _editListQuestionEventEntityUuid = newEntity ^. uuid
            , _editListQuestionEventTitle = diffField (oldEntity ^. title) (newEntity ^. title)
            , _editListQuestionEventText = diffField (oldEntity ^. text) (newEntity ^. text)
            , _editListQuestionEventRequiredPhaseUuid =
                diffField (oldEntity ^. requiredPhaseUuid) (newEntity ^. requiredPhaseUuid)
            , _editListQuestionEventAnnotations = diffField (oldEntity ^. annotations) (newEntity ^. annotations)
            , _editListQuestionEventTagUuids = diffField (oldEntity ^. tagUuids) (newEntity ^. tagUuids)
            , _editListQuestionEventExpertUuids = diffField (oldEntity ^. expertUuids) (newEntity ^. expertUuids)
            , _editListQuestionEventReferenceUuids =
                diffField (oldEntity ^. referenceUuids) (newEntity ^. referenceUuids)
            , _editListQuestionEventItemTemplateQuestionUuids =
                diffListField
                  (oldKm, newKm)
                  (oldEntity ^. itemTemplateQuestionUuids)
                  (newEntity ^. itemTemplateQuestionUuids)
                  questionsM
            , _editListQuestionEventCreatedAt = now
            }
    if isEmptyEvent event
      then return . Just . EditQuestionEvent' . EditListQuestionEvent' $ event
      else return Nothing
  createEditEvent (oldKm, newKm) parentUuid (ValueQuestion' oldEntity) (ValueQuestion' newEntity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let event =
          EditValueQuestionEvent
            { _editValueQuestionEventUuid = eventUuid
            , _editValueQuestionEventParentUuid = parentUuid
            , _editValueQuestionEventEntityUuid = newEntity ^. uuid
            , _editValueQuestionEventTitle = diffField (oldEntity ^. title) (newEntity ^. title)
            , _editValueQuestionEventText = diffField (oldEntity ^. text) (newEntity ^. text)
            , _editValueQuestionEventRequiredPhaseUuid =
                diffField (oldEntity ^. requiredPhaseUuid) (newEntity ^. requiredPhaseUuid)
            , _editValueQuestionEventAnnotations = diffField (oldEntity ^. annotations) (newEntity ^. annotations)
            , _editValueQuestionEventTagUuids = diffField (oldEntity ^. tagUuids) (newEntity ^. tagUuids)
            , _editValueQuestionEventExpertUuids = diffField (oldEntity ^. expertUuids) (newEntity ^. expertUuids)
            , _editValueQuestionEventReferenceUuids =
                diffField (oldEntity ^. referenceUuids) (newEntity ^. referenceUuids)
            , _editValueQuestionEventValueType = diffField (oldEntity ^. valueType) (newEntity ^. valueType)
            , _editValueQuestionEventCreatedAt = now
            }
    if isEmptyEvent event
      then return . Just . EditQuestionEvent' . EditValueQuestionEvent' $ event
      else return Nothing
  createEditEvent (oldKm, newKm) parentUuid (IntegrationQuestion' oldEntity) (IntegrationQuestion' newEntity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let event =
          EditIntegrationQuestionEvent
            { _editIntegrationQuestionEventUuid = eventUuid
            , _editIntegrationQuestionEventParentUuid = parentUuid
            , _editIntegrationQuestionEventEntityUuid = newEntity ^. uuid
            , _editIntegrationQuestionEventTitle = diffField (oldEntity ^. title) (newEntity ^. title)
            , _editIntegrationQuestionEventText = diffField (oldEntity ^. text) (newEntity ^. text)
            , _editIntegrationQuestionEventRequiredPhaseUuid =
                diffField (oldEntity ^. requiredPhaseUuid) (newEntity ^. requiredPhaseUuid)
            , _editIntegrationQuestionEventAnnotations = diffField (oldEntity ^. annotations) (newEntity ^. annotations)
            , _editIntegrationQuestionEventTagUuids = diffField (oldEntity ^. tagUuids) (newEntity ^. tagUuids)
            , _editIntegrationQuestionEventExpertUuids = diffField (oldEntity ^. expertUuids) (newEntity ^. expertUuids)
            , _editIntegrationQuestionEventReferenceUuids =
                diffField (oldEntity ^. referenceUuids) (newEntity ^. referenceUuids)
            , _editIntegrationQuestionEventIntegrationUuid =
                diffField (oldEntity ^. integrationUuid) (newEntity ^. integrationUuid)
            , _editIntegrationQuestionEventProps = diffField (oldEntity ^. props) (newEntity ^. props)
            , _editIntegrationQuestionEventCreatedAt = now
            }
    if isEmptyEvent event
      then return . Just . EditQuestionEvent' . EditIntegrationQuestionEvent' $ event
      else return Nothing
  createDeleteEvent parentUuid entity = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      DeleteQuestionEvent' $
      DeleteQuestionEvent
        { _deleteQuestionEventUuid = eventUuid
        , _deleteQuestionEventParentUuid = parentUuid
        , _deleteQuestionEventEntityUuid = entity ^. uuid'
        , _deleteQuestionEventCreatedAt = now
        }
