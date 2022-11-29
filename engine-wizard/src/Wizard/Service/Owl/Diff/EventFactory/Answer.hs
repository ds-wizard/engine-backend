module Wizard.Service.Owl.Diff.EventFactory.Answer where

import Control.Monad.Reader (liftIO)
import Data.Time

import Shared.Model.Event.Answer.AnswerEvent
import Shared.Model.Event.Event
import Shared.Model.Event.EventField
import Shared.Model.Event.EventUtil
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Shared.Util.Uuid
import Wizard.Service.Owl.Diff.Accessor.Accessor
import Wizard.Service.Owl.Diff.EventFactory.EventFactory

instance EventFactory Answer where
  createAddEvent parentUuid entity = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      AddAnswerEvent' $
        AddAnswerEvent
          { uuid = eventUuid
          , parentUuid = parentUuid
          , entityUuid = entity.uuid
          , aLabel = entity.aLabel
          , advice = entity.advice
          , annotations = entity.annotations
          , metricMeasures = entity.metricMeasures
          , createdAt = now
          }
  createEditEvent (oldKm, newKm) parentUuid oldEntity newEntity = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let event =
          EditAnswerEvent
            { uuid = eventUuid
            , parentUuid = parentUuid
            , entityUuid = newEntity.uuid
            , aLabel = diffField oldEntity.aLabel newEntity.aLabel
            , advice = diffField oldEntity.advice newEntity.advice
            , annotations = diffField oldEntity.annotations newEntity.annotations
            , followUpUuids =
                diffListField (oldKm, newKm) oldEntity.followUpUuids newEntity.followUpUuids getQuestionsM
            , metricMeasures = diffField oldEntity.metricMeasures newEntity.metricMeasures
            , createdAt = now
            }
    if isEmptyEvent event
      then return . Just . EditAnswerEvent' $ event
      else return Nothing
  createDeleteEvent parentUuid entity = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      DeleteAnswerEvent' $
        DeleteAnswerEvent
          { uuid = eventUuid
          , parentUuid = parentUuid
          , entityUuid = entity.uuid
          , createdAt = now
          }
