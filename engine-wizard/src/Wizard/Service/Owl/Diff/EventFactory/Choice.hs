module Wizard.Service.Owl.Diff.EventFactory.Choice where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Time

import LensesConfig
import Shared.Model.Event.Choice.ChoiceEvent
import Shared.Model.Event.Event
import Shared.Model.Event.EventField
import Shared.Model.Event.EventUtil
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses ()
import Shared.Util.Uuid
import Wizard.Service.Owl.Diff.EventFactory.EventFactory

instance EventFactory Choice where
  createAddEvent parentUuid entity = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      AddChoiceEvent' $
      AddChoiceEvent
        { _addChoiceEventUuid = eventUuid
        , _addChoiceEventParentUuid = parentUuid
        , _addChoiceEventEntityUuid = entity ^. uuid
        , _addChoiceEventLabel = entity ^. label
        , _addChoiceEventAnnotations = entity ^. annotations
        , _addChoiceEventCreatedAt = now
        }
  createEditEvent (oldKm, newKm) parentUuid oldEntity newEntity = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let event =
          EditChoiceEvent
            { _editChoiceEventUuid = eventUuid
            , _editChoiceEventParentUuid = parentUuid
            , _editChoiceEventEntityUuid = newEntity ^. uuid
            , _editChoiceEventLabel = diffField (oldEntity ^. label) (newEntity ^. label)
            , _editChoiceEventAnnotations = diffField (oldEntity ^. annotations) (newEntity ^. annotations)
            , _editChoiceEventCreatedAt = now
            }
    if isEmptyEvent event
      then return . Just . EditChoiceEvent' $ event
      else return Nothing
  createDeleteEvent parentUuid entity = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      DeleteChoiceEvent' $
      DeleteChoiceEvent
        { _deleteChoiceEventUuid = eventUuid
        , _deleteChoiceEventParentUuid = parentUuid
        , _deleteChoiceEventEntityUuid = entity ^. uuid
        , _deleteChoiceEventCreatedAt = now
        }
