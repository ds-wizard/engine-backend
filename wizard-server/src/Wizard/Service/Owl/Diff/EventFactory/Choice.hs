module Wizard.Service.Owl.Diff.EventFactory.Choice where

import Control.Monad.Reader (liftIO)
import Data.Time

import Shared.Common.Util.Uuid
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Choice.ChoiceEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventUtil
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses ()
import Wizard.Service.Owl.Diff.EventFactory.EventFactory

instance EventFactory Choice where
  createAddEvent parentUuid entity = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      KnowledgeModelEvent
        { uuid = eventUuid
        , parentUuid = parentUuid
        , entityUuid = entity.uuid
        , content =
            AddChoiceEvent' $
              AddChoiceEvent
                { aLabel = entity.aLabel
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
                EditChoiceEvent' $
                  EditChoiceEvent
                    { aLabel = diffField oldEntity.aLabel newEntity.aLabel
                    , annotations = diffField oldEntity.annotations newEntity.annotations
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
        , content = DeleteChoiceEvent' DeleteChoiceEvent
        , createdAt = now
        }
