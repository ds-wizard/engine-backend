module Wizard.Service.Owl.Diff.EventFactory.Question where

import Control.Monad.Reader (liftIO)
import Data.Time

import Shared.Common.Model.Common.Lens
import Shared.Common.Util.Uuid
import Wizard.Service.Owl.Diff.Accessor.Accessor
import Wizard.Service.Owl.Diff.EventFactory.EventFactory
import WizardLib.KnowledgeModel.Model.Common.Lens
import WizardLib.KnowledgeModel.Model.Event.Event
import WizardLib.KnowledgeModel.Model.Event.EventField
import WizardLib.KnowledgeModel.Model.Event.EventUtil
import WizardLib.KnowledgeModel.Model.Event.Question.QuestionEvent
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

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
            , validations = entity.validations
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
  createAddEvent _ _ = error "Add event is not applicable for this type of question"
  createEditEvent (oldKm, newKm) parentUuid (OptionsQuestion' newEntity) (OptionsQuestion' oldEntity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let event =
          EditOptionsQuestionEvent
            { uuid = eventUuid
            , parentUuid = parentUuid
            , entityUuid = oldEntity.uuid
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
  createEditEvent (oldKm, newKm) parentUuid (MultiChoiceQuestion' newEntity) (MultiChoiceQuestion' oldEntity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let event =
          EditMultiChoiceQuestionEvent
            { uuid = eventUuid
            , parentUuid = parentUuid
            , entityUuid = oldEntity.uuid
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
  createEditEvent (oldKm, newKm) parentUuid (ListQuestion' newEntity) (ListQuestion' oldEntity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let event =
          EditListQuestionEvent
            { uuid = eventUuid
            , parentUuid = parentUuid
            , entityUuid = oldEntity.uuid
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
  createEditEvent (oldKm, newKm) parentUuid (ValueQuestion' newEntity) (ValueQuestion' oldEntity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let event =
          EditValueQuestionEvent
            { uuid = eventUuid
            , parentUuid = parentUuid
            , entityUuid = oldEntity.uuid
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
            , validations = diffField oldEntity.validations newEntity.validations
            , createdAt = now
            }
    if isEmptyEvent event
      then return . Just . EditQuestionEvent' . EditValueQuestionEvent' $ event
      else return Nothing
  createEditEvent (oldKm, newKm) parentUuid (IntegrationQuestion' newEntity) (IntegrationQuestion' oldEntity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let event =
          EditIntegrationQuestionEvent
            { uuid = eventUuid
            , parentUuid = parentUuid
            , entityUuid = oldEntity.uuid
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
  createEditEvent _ _ _ _ = error "Edit event is not applicable for this type of question"
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
