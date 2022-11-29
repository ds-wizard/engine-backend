module Wizard.Service.Owl.Diff.EventFactory.Chapter where

import Control.Monad.Reader (liftIO)
import Data.Time

import Shared.Model.Event.Chapter.ChapterEvent
import Shared.Model.Event.Event
import Shared.Model.Event.EventField
import Shared.Model.Event.EventUtil
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Shared.Util.Uuid
import Wizard.Service.Owl.Diff.Accessor.Accessor
import Wizard.Service.Owl.Diff.EventFactory.EventFactory

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
