module Wizard.Service.Owl.Diff.EventFactory.Chapter where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Time

import LensesConfig
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
        { _addChapterEventUuid = eventUuid
        , _addChapterEventParentUuid = parentUuid
        , _addChapterEventEntityUuid = entity ^. uuid
        , _addChapterEventTitle = entity ^. title
        , _addChapterEventText = entity ^. text
        , _addChapterEventAnnotations = entity ^. annotations
        , _addChapterEventCreatedAt = now
        }
  createEditEvent (oldKm, newKm) parentUuid oldEntity newEntity = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let event =
          EditChapterEvent
            { _editChapterEventUuid = eventUuid
            , _editChapterEventParentUuid = parentUuid
            , _editChapterEventEntityUuid = newEntity ^. uuid
            , _editChapterEventTitle = diffField (oldEntity ^. title) (newEntity ^. title)
            , _editChapterEventText = diffField (oldEntity ^. text) (newEntity ^. text)
            , _editChapterEventAnnotations = diffField (oldEntity ^. annotations) (newEntity ^. annotations)
            , _editChapterEventQuestionUuids =
                diffListField (oldKm, newKm) (oldEntity ^. questionUuids) (newEntity ^. questionUuids) questionsM
            , _editChapterEventCreatedAt = now
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
        { _deleteChapterEventUuid = eventUuid
        , _deleteChapterEventParentUuid = parentUuid
        , _deleteChapterEventEntityUuid = entity ^. uuid
        , _deleteChapterEventCreatedAt = now
        }
