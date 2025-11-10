module Wizard.Service.Owl.Diff.EventFactory.Chapter where

import Control.Monad.Reader (liftIO)
import Data.Time

import Shared.Common.Util.Uuid
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Chapter.ChapterEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventUtil
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Service.Owl.Diff.Accessor.Accessor
import Wizard.Service.Owl.Diff.EventFactory.EventFactory

instance EventFactory Chapter where
  createAddEvent parentUuid entity = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      KnowledgeModelEvent
        { uuid = eventUuid
        , parentUuid = parentUuid
        , entityUuid = entity.uuid
        , content =
            AddChapterEvent' $
              AddChapterEvent
                { title = entity.title
                , text = entity.text
                , annotations = entity.annotations
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
                EditChapterEvent' $
                  EditChapterEvent
                    { title = diffField oldEntity.title newEntity.title
                    , text = diffField oldEntity.text newEntity.text
                    , annotations = diffField oldEntity.annotations newEntity.annotations
                    , questionUuids =
                        diffListField (oldKm, newKm) oldEntity.questionUuids newEntity.questionUuids getQuestionsM
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
        , content = DeleteChapterEvent' DeleteChapterEvent
        , createdAt = now
        }
