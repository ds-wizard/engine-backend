module Wizard.Service.Owl.Diff.EventFactory.Choice where

import Control.Monad.Reader (liftIO)
import Data.Time

import Shared.Common.Util.Uuid
import Wizard.Service.Owl.Diff.EventFactory.EventFactory
import WizardLib.KnowledgeModel.Model.Event.Choice.ChoiceEvent
import WizardLib.KnowledgeModel.Model.Event.Event
import WizardLib.KnowledgeModel.Model.Event.EventField
import WizardLib.KnowledgeModel.Model.Event.EventUtil
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses ()

instance EventFactory Choice where
  createAddEvent parentUuid entity = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      AddChoiceEvent' $
        AddChoiceEvent
          { uuid = eventUuid
          , parentUuid = parentUuid
          , entityUuid = entity.uuid
          , aLabel = entity.aLabel
          , annotations = entity.annotations
          , createdAt = now
          }
  createEditEvent (oldKm, newKm) parentUuid oldEntity newEntity = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let event =
          EditChoiceEvent
            { uuid = eventUuid
            , parentUuid = parentUuid
            , entityUuid = newEntity.uuid
            , aLabel = diffField oldEntity.aLabel newEntity.aLabel
            , annotations = diffField oldEntity.annotations newEntity.annotations
            , createdAt = now
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
          { uuid = eventUuid
          , parentUuid = parentUuid
          , entityUuid = entity.uuid
          , createdAt = now
          }
