module Wizard.Service.Owl.Diff.EventFactory.Chapter where

import Control.Monad.Reader (liftIO)
import Data.Time

import Shared.Common.Util.Uuid
import Wizard.Service.Owl.Diff.Accessor.Accessor
import Wizard.Service.Owl.Diff.EventFactory.EventFactory
import WizardLib.KnowledgeModel.Model.Event.Chapter.ChapterEvent
import WizardLib.KnowledgeModel.Model.Event.Event
import WizardLib.KnowledgeModel.Model.Event.EventField
import WizardLib.KnowledgeModel.Model.Event.EventUtil
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses

instance EventFactory Chapter where
  createAddEvent parentUuid entity = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      AddChapterEvent' $
        AddChapterEvent
          { uuid = eventUuid
          , parentUuid = parentUuid
          , entityUuid = entity.uuid
          , title = entity.title
          , text = entity.text
          , annotations = entity.annotations
          , createdAt = now
          }
  createEditEvent (oldKm, newKm) parentUuid oldEntity newEntity = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let event =
          EditChapterEvent
            { uuid = eventUuid
            , parentUuid = parentUuid
            , entityUuid = newEntity.uuid
            , title = diffField oldEntity.title newEntity.title
            , text = diffField oldEntity.text newEntity.text
            , annotations = diffField oldEntity.annotations newEntity.annotations
            , questionUuids =
                diffListField (oldKm, newKm) oldEntity.questionUuids newEntity.questionUuids getQuestionsM
            , createdAt = now
            }
    if isEmptyEvent event
      then return . Just . EditChapterEvent' $ event
      else return Nothing
  createDeleteEvent parentUuid entity = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      DeleteChapterEvent' $
        DeleteChapterEvent
          { uuid = eventUuid
          , parentUuid = parentUuid
          , entityUuid = entity.uuid
          , createdAt = now
          }
