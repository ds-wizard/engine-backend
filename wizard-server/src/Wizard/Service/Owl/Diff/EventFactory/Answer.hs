module Wizard.Service.Owl.Diff.EventFactory.Answer where

import Control.Monad.Reader (liftIO)
import Data.Time

import Shared.Common.Util.Uuid
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Answer.AnswerEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventUtil
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Service.Owl.Diff.Accessor.Accessor
import Wizard.Service.Owl.Diff.EventFactory.EventFactory

instance EventFactory Answer where
  createAddEvent parentUuid entity = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      KnowledgeModelEvent
        { uuid = eventUuid
        , parentUuid = parentUuid
        , entityUuid = entity.uuid
        , content =
            AddAnswerEvent' $
              AddAnswerEvent
                { aLabel = entity.aLabel
                , advice = entity.advice
                , annotations = entity.annotations
                , metricMeasures = entity.metricMeasures
                }
        , createdAt = now
        }
  createEditEvent (oldKm, newKm) parentUuid oldEntity newEntity = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let event =
          KnowledgeModelEvent
            { uuid = eventUuid
            , parentUuid = parentUuid
            , entityUuid = newEntity.uuid
            , content =
                EditAnswerEvent' $
                  EditAnswerEvent
                    { aLabel = diffField oldEntity.aLabel newEntity.aLabel
                    , advice = diffField oldEntity.advice newEntity.advice
                    , annotations = diffField oldEntity.annotations newEntity.annotations
                    , followUpUuids =
                        diffListField (oldKm, newKm) oldEntity.followUpUuids newEntity.followUpUuids getQuestionsM
                    , metricMeasures = diffField oldEntity.metricMeasures newEntity.metricMeasures
                    }
            , createdAt = now
            }
    if isEmptyEvent event.content
      then return . Just $ event
      else return Nothing
  createDeleteEvent parentUuid entity = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      KnowledgeModelEvent
        { uuid = eventUuid
        , parentUuid = parentUuid
        , entityUuid = entity.uuid
        , content = DeleteAnswerEvent' DeleteAnswerEvent
        , createdAt = now
        }
