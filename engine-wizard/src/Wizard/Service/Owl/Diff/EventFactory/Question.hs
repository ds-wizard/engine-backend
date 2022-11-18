module Wizard.Service.Owl.Diff.EventFactory.Question where

import Control.Monad.Reader (liftIO)
import Data.Time

import Shared.Model.Common.Lens
import Shared.Model.Event.Event
import Shared.Model.Event.EventField
import Shared.Model.Event.EventUtil
import Shared.Model.Event.Question.QuestionEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
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
            { uuid = eventUuid
            , parentUuid = parentUuid
            , entityUuid = entity.uuid
            , title = entity.title
            , text = entity.text
            , requiredPhaseUuid = entity.requiredPhaseUuid
            , annotations = entity.annotations
            , tagUuids = entity.tagUuids
            , createdAt = now
            }
  createAddEvent parentUuid (MultiChoiceQuestion' entity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      AddQuestionEvent' $
        AddMultiChoiceQuestionEvent' $
          AddMultiChoiceQuestionEvent
            { uuid = eventUuid
            , parentUuid = parentUuid
            , entityUuid = entity.uuid
            , title = entity.title
            , text = entity.text
            , requiredPhaseUuid = entity.requiredPhaseUuid
            , annotations = entity.annotations
            , tagUuids = entity.tagUuids
            , createdAt = now
            }
  createAddEvent parentUuid (ListQuestion' entity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      AddQuestionEvent' $
        AddListQuestionEvent' $
          AddListQuestionEvent
            { uuid = eventUuid
            , parentUuid = parentUuid
            , entityUuid = entity.uuid
            , title = entity.title
            , text = entity.text
            , requiredPhaseUuid = entity.requiredPhaseUuid
            , annotations = entity.annotations
            , tagUuids = entity.tagUuids
            , createdAt = now
            }
  createAddEvent parentUuid (ValueQuestion' entity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      AddQuestionEvent' $
        AddValueQuestionEvent' $
          AddValueQuestionEvent
            { uuid = eventUuid
            , parentUuid = parentUuid
            , entityUuid = entity.uuid
            , title = entity.title
            , text = entity.text
            , requiredPhaseUuid = entity.requiredPhaseUuid
            , annotations = entity.annotations
            , tagUuids = entity.tagUuids
            , valueType = entity.valueType
            , createdAt = now
            }
  createAddEvent parentUuid (IntegrationQuestion' entity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      AddQuestionEvent' $
        AddIntegrationQuestionEvent' $
          AddIntegrationQuestionEvent
            { uuid = eventUuid
            , parentUuid = parentUuid
            , entityUuid = entity.uuid
            , title = entity.title
            , text = entity.text
            , requiredPhaseUuid = entity.requiredPhaseUuid
            , annotations = entity.annotations
            , tagUuids = entity.tagUuids
            , integrationUuid = entity.integrationUuid
            , props = entity.props
            , createdAt = now
            }
  createEditEvent (oldKm, newKm) parentUuid (OptionsQuestion' oldEntity) (OptionsQuestion' newEntity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let event =
          EditOptionsQuestionEvent
            { uuid = eventUuid
            , parentUuid = parentUuid
            , entityUuid = newEntity.uuid
            , title = diffField oldEntity.title newEntity.title
            , text = diffField oldEntity.text newEntity.text
            , requiredPhaseUuid =
                diffField oldEntity.requiredPhaseUuid newEntity.requiredPhaseUuid
            , annotations = diffField oldEntity.annotations newEntity.annotations
            , tagUuids = diffField oldEntity.tagUuids newEntity.tagUuids
            , expertUuids = diffField oldEntity.expertUuids newEntity.expertUuids
            , referenceUuids =
                diffField oldEntity.referenceUuids newEntity.referenceUuids
            , answerUuids =
                diffListField (oldKm, newKm) oldEntity.answerUuids newEntity.answerUuids getAnswersM
            , createdAt = now
            }
    if isEmptyEvent event
      then return . Just . EditQuestionEvent' . EditOptionsQuestionEvent' $ event
      else return Nothing
  createEditEvent (oldKm, newKm) parentUuid (MultiChoiceQuestion' oldEntity) (MultiChoiceQuestion' newEntity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let event =
          EditMultiChoiceQuestionEvent
            { uuid = eventUuid
            , parentUuid = parentUuid
            , entityUuid = newEntity.uuid
            , title = diffField oldEntity.title newEntity.title
            , text = diffField oldEntity.text newEntity.text
            , requiredPhaseUuid =
                diffField oldEntity.requiredPhaseUuid newEntity.requiredPhaseUuid
            , annotations = diffField oldEntity.annotations newEntity.annotations
            , tagUuids = diffField oldEntity.tagUuids newEntity.tagUuids
            , expertUuids = diffField oldEntity.expertUuids newEntity.expertUuids
            , referenceUuids =
                diffField oldEntity.referenceUuids newEntity.referenceUuids
            , choiceUuids =
                diffListField (oldKm, newKm) oldEntity.choiceUuids newEntity.choiceUuids getChoicesM
            , createdAt = now
            }
    if isEmptyEvent event
      then return . Just . EditQuestionEvent' . EditMultiChoiceQuestionEvent' $ event
      else return Nothing
  createEditEvent (oldKm, newKm) parentUuid (ListQuestion' oldEntity) (ListQuestion' newEntity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let event =
          EditListQuestionEvent
            { uuid = eventUuid
            , parentUuid = parentUuid
            , entityUuid = newEntity.uuid
            , title = diffField oldEntity.title newEntity.title
            , text = diffField oldEntity.text newEntity.text
            , requiredPhaseUuid =
                diffField oldEntity.requiredPhaseUuid newEntity.requiredPhaseUuid
            , annotations = diffField oldEntity.annotations newEntity.annotations
            , tagUuids = diffField oldEntity.tagUuids newEntity.tagUuids
            , expertUuids = diffField oldEntity.expertUuids newEntity.expertUuids
            , referenceUuids =
                diffField oldEntity.referenceUuids newEntity.referenceUuids
            , itemTemplateQuestionUuids =
                diffListField
                  (oldKm, newKm)
                  oldEntity.itemTemplateQuestionUuids
                  newEntity.itemTemplateQuestionUuids
                  getQuestionsM
            , createdAt = now
            }
    if isEmptyEvent event
      then return . Just . EditQuestionEvent' . EditListQuestionEvent' $ event
      else return Nothing
  createEditEvent (oldKm, newKm) parentUuid (ValueQuestion' oldEntity) (ValueQuestion' newEntity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let event =
          EditValueQuestionEvent
            { uuid = eventUuid
            , parentUuid = parentUuid
            , entityUuid = newEntity.uuid
            , title = diffField oldEntity.title newEntity.title
            , text = diffField oldEntity.text newEntity.text
            , requiredPhaseUuid =
                diffField oldEntity.requiredPhaseUuid newEntity.requiredPhaseUuid
            , annotations = diffField oldEntity.annotations newEntity.annotations
            , tagUuids = diffField oldEntity.tagUuids newEntity.tagUuids
            , expertUuids = diffField oldEntity.expertUuids newEntity.expertUuids
            , referenceUuids =
                diffField oldEntity.referenceUuids newEntity.referenceUuids
            , valueType = diffField oldEntity.valueType newEntity.valueType
            , createdAt = now
            }
    if isEmptyEvent event
      then return . Just . EditQuestionEvent' . EditValueQuestionEvent' $ event
      else return Nothing
  createEditEvent (oldKm, newKm) parentUuid (IntegrationQuestion' oldEntity) (IntegrationQuestion' newEntity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let event =
          EditIntegrationQuestionEvent
            { uuid = eventUuid
            , parentUuid = parentUuid
            , entityUuid = newEntity.uuid
            , title = diffField oldEntity.title newEntity.title
            , text = diffField oldEntity.text newEntity.text
            , requiredPhaseUuid =
                diffField oldEntity.requiredPhaseUuid newEntity.requiredPhaseUuid
            , annotations = diffField oldEntity.annotations newEntity.annotations
            , tagUuids = diffField oldEntity.tagUuids newEntity.tagUuids
            , expertUuids = diffField oldEntity.expertUuids newEntity.expertUuids
            , referenceUuids =
                diffField oldEntity.referenceUuids newEntity.referenceUuids
            , integrationUuid =
                diffField oldEntity.integrationUuid newEntity.integrationUuid
            , props = diffField oldEntity.props newEntity.props
            , createdAt = now
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
          { uuid = eventUuid
          , parentUuid = parentUuid
          , entityUuid = getUuid entity
          , createdAt = now
          }
