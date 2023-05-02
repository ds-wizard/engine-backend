module Wizard.Service.Owl.Diff.EventFactory.Answer where

import Control.Monad.Reader (liftIO)
import Data.Time

import Shared.Common.Util.Uuid
import Wizard.Service.Owl.Diff.Accessor.Accessor
import Wizard.Service.Owl.Diff.EventFactory.EventFactory
import WizardLib.KnowledgeModel.Model.Event.Answer.AnswerEvent
import WizardLib.KnowledgeModel.Model.Event.Event
import WizardLib.KnowledgeModel.Model.Event.EventField
import WizardLib.KnowledgeModel.Model.Event.EventUtil
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses

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
