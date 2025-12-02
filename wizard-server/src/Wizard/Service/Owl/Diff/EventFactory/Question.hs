module Wizard.Service.Owl.Diff.EventFactory.Question where

import Control.Monad.Reader (liftIO)
import Data.Time

import Shared.Common.Model.Common.Lens
import Shared.Common.Util.Uuid
import Shared.KnowledgeModel.Model.Common.Lens
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventUtil
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Question.QuestionEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.Owl.Diff.Accessor.Accessor
import Wizard.Service.Owl.Diff.EventFactory.EventFactory

instance EventFactory Question where
  createAddEvent parentUuid (OptionsQuestion' entity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      KnowledgeModelEvent
        { uuid = eventUuid
        , parentUuid = parentUuid
        , entityUuid = entity.uuid
        , content =
            AddQuestionEvent' $
              AddOptionsQuestionEvent' $
                AddOptionsQuestionEvent
                  { title = entity.title
                  , text = entity.text
                  , requiredPhaseUuid = entity.requiredPhaseUuid
                  , annotations = entity.annotations
                  , tagUuids = entity.tagUuids
                  }
        , createdAt = now
        }
  createAddEvent parentUuid (MultiChoiceQuestion' entity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      KnowledgeModelEvent
        { uuid = eventUuid
        , parentUuid = parentUuid
        , entityUuid = entity.uuid
        , content =
            AddQuestionEvent' $
              AddMultiChoiceQuestionEvent' $
                AddMultiChoiceQuestionEvent
                  { title = entity.title
                  , text = entity.text
                  , requiredPhaseUuid = entity.requiredPhaseUuid
                  , annotations = entity.annotations
                  , tagUuids = entity.tagUuids
                  }
        , createdAt = now
        }
  createAddEvent parentUuid (ListQuestion' entity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      KnowledgeModelEvent
        { uuid = eventUuid
        , parentUuid = parentUuid
        , entityUuid = entity.uuid
        , content =
            AddQuestionEvent' $
              AddListQuestionEvent' $
                AddListQuestionEvent
                  { title = entity.title
                  , text = entity.text
                  , requiredPhaseUuid = entity.requiredPhaseUuid
                  , annotations = entity.annotations
                  , tagUuids = entity.tagUuids
                  }
        , createdAt = now
        }
  createAddEvent parentUuid (ValueQuestion' entity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      KnowledgeModelEvent
        { uuid = eventUuid
        , parentUuid = parentUuid
        , entityUuid = entity.uuid
        , content =
            AddQuestionEvent' $
              AddValueQuestionEvent' $
                AddValueQuestionEvent
                  { title = entity.title
                  , text = entity.text
                  , requiredPhaseUuid = entity.requiredPhaseUuid
                  , annotations = entity.annotations
                  , tagUuids = entity.tagUuids
                  , valueType = entity.valueType
                  , validations = entity.validations
                  }
        , createdAt = now
        }
  createAddEvent parentUuid (IntegrationQuestion' entity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      KnowledgeModelEvent
        { uuid = eventUuid
        , parentUuid = parentUuid
        , entityUuid = entity.uuid
        , content =
            AddQuestionEvent' $
              AddIntegrationQuestionEvent' $
                AddIntegrationQuestionEvent
                  { title = entity.title
                  , text = entity.text
                  , requiredPhaseUuid = entity.requiredPhaseUuid
                  , annotations = entity.annotations
                  , tagUuids = entity.tagUuids
                  , integrationUuid = entity.integrationUuid
                  , variables = entity.variables
                  }
        , createdAt = now
        }
  createAddEvent _ _ = error "Add event is not applicable for this type of question"
  createEditEvent (oldKm, newKm) parentUuid (OptionsQuestion' newEntity) (OptionsQuestion' oldEntity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let event =
          KnowledgeModelEvent
            { uuid = eventUuid
            , parentUuid = parentUuid
            , entityUuid = oldEntity.uuid
            , content =
                EditQuestionEvent' $
                  EditOptionsQuestionEvent' $
                    EditOptionsQuestionEvent
                      { title = diffField oldEntity.title newEntity.title
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
                      }
            , createdAt = now
            }
    if isEmptyEvent event.content
      then return . Just $ event
      else return Nothing
  createEditEvent (oldKm, newKm) parentUuid (MultiChoiceQuestion' newEntity) (MultiChoiceQuestion' oldEntity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let event =
          KnowledgeModelEvent
            { uuid = eventUuid
            , parentUuid = parentUuid
            , entityUuid = oldEntity.uuid
            , content =
                EditQuestionEvent' $
                  EditMultiChoiceQuestionEvent' $
                    EditMultiChoiceQuestionEvent
                      { title = diffField oldEntity.title newEntity.title
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
                      }
            , createdAt = now
            }
    if isEmptyEvent event.content
      then return . Just $ event
      else return Nothing
  createEditEvent (oldKm, newKm) parentUuid (ListQuestion' newEntity) (ListQuestion' oldEntity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let event =
          KnowledgeModelEvent
            { uuid = eventUuid
            , parentUuid = parentUuid
            , entityUuid = oldEntity.uuid
            , content =
                EditQuestionEvent' $
                  EditListQuestionEvent' $
                    EditListQuestionEvent
                      { title = diffField oldEntity.title newEntity.title
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
                      }
            , createdAt = now
            }
    if isEmptyEvent event.content
      then return . Just $ event
      else return Nothing
  createEditEvent (oldKm, newKm) parentUuid (ValueQuestion' newEntity) (ValueQuestion' oldEntity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let event =
          KnowledgeModelEvent
            { uuid = eventUuid
            , parentUuid = parentUuid
            , entityUuid = oldEntity.uuid
            , content =
                EditQuestionEvent' $
                  EditValueQuestionEvent' $
                    EditValueQuestionEvent
                      { title = diffField oldEntity.title newEntity.title
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
                      }
            , createdAt = now
            }
    if isEmptyEvent event.content
      then return . Just $ event
      else return Nothing
  createEditEvent (oldKm, newKm) parentUuid (IntegrationQuestion' newEntity) (IntegrationQuestion' oldEntity) = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let event =
          KnowledgeModelEvent
            { uuid = eventUuid
            , parentUuid = parentUuid
            , entityUuid = oldEntity.uuid
            , content =
                EditQuestionEvent' $
                  EditIntegrationQuestionEvent' $
                    EditIntegrationQuestionEvent
                      { title = diffField oldEntity.title newEntity.title
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
                      , variables = diffField oldEntity.variables newEntity.variables
                      }
            , createdAt = now
            }
    if isEmptyEvent event.content
      then return . Just $ event
      else return Nothing
  createEditEvent _ _ _ _ = error "Edit event is not applicable for this type of question"
  createDeleteEvent parentUuid entity = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      KnowledgeModelEvent
        { uuid = eventUuid
        , parentUuid = parentUuid
        , entityUuid = getUuid entity
        , content = DeleteQuestionEvent' DeleteQuestionEvent
        , createdAt = now
        }
