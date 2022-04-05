module Wizard.Service.Owl.Diff.EventFactory.Answer where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Time

import LensesConfig
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
        { _addAnswerEventUuid = eventUuid
        , _addAnswerEventParentUuid = parentUuid
        , _addAnswerEventEntityUuid = entity ^. uuid
        , _addAnswerEventLabel = entity ^. label
        , _addAnswerEventAdvice = entity ^. advice
        , _addAnswerEventAnnotations = entity ^. annotations
        , _addAnswerEventMetricMeasures = entity ^. metricMeasures
        , _addAnswerEventCreatedAt = now
        }
  createEditEvent (oldKm, newKm) parentUuid oldEntity newEntity = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let event =
          EditAnswerEvent
            { _editAnswerEventUuid = eventUuid
            , _editAnswerEventParentUuid = parentUuid
            , _editAnswerEventEntityUuid = newEntity ^. uuid
            , _editAnswerEventLabel = diffField (oldEntity ^. label) (newEntity ^. label)
            , _editAnswerEventAdvice = diffField (oldEntity ^. advice) (newEntity ^. advice)
            , _editAnswerEventAnnotations = diffField (oldEntity ^. annotations) (newEntity ^. annotations)
            , _editAnswerEventFollowUpUuids =
                diffListField (oldKm, newKm) (oldEntity ^. followUpUuids) (newEntity ^. followUpUuids) questionsM
            , _editAnswerEventMetricMeasures = diffField (oldEntity ^. metricMeasures) (newEntity ^. metricMeasures)
            , _editAnswerEventCreatedAt = now
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
        { _deleteAnswerEventUuid = eventUuid
        , _deleteAnswerEventParentUuid = parentUuid
        , _deleteAnswerEventEntityUuid = entity ^. uuid
        , _deleteAnswerEventCreatedAt = now
        }
