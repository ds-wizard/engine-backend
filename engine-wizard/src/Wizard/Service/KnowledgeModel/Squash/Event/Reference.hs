module Wizard.Service.KnowledgeModel.Squash.Event.Reference where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Event.Reference.ReferenceEvent
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditReferenceEvent where
  isSimpleEventSquashApplicable _ = True
  --  --------------------------------------
  isTypeChanged (EditResourcePageReferenceEvent' oldEvent) (EditResourcePageReferenceEvent' newEvent) = False
  isTypeChanged (EditURLReferenceEvent' oldEvent) (EditURLReferenceEvent' newEvent) = False
  isTypeChanged (EditCrossReferenceEvent' oldEvent) (EditCrossReferenceEvent' newEvent) = False
  isTypeChanged _ _ = True
  --  --------------------------------------
  simpleSquashEvent (EditResourcePageReferenceEvent' oldEvent) (EditResourcePageReferenceEvent' newEvent) =
    EditResourcePageReferenceEvent' $
    EditResourcePageReferenceEvent
      { _editResourcePageReferenceEventUuid = newEvent ^. uuid
      , _editResourcePageReferenceEventParentUuid = newEvent ^. parentUuid
      , _editResourcePageReferenceEventEntityUuid = newEvent ^. entityUuid
      , _editResourcePageReferenceEventShortUuid = applyValue oldEvent newEvent shortUuid
      , _editResourcePageReferenceEventAnnotations = applyValue oldEvent newEvent annotations
      , _editResourcePageReferenceEventCreatedAt = newEvent ^. createdAt
      }
  simpleSquashEvent (EditURLReferenceEvent' oldEvent) (EditURLReferenceEvent' newEvent) =
    EditURLReferenceEvent' $
    EditURLReferenceEvent
      { _editURLReferenceEventUuid = newEvent ^. uuid
      , _editURLReferenceEventParentUuid = newEvent ^. parentUuid
      , _editURLReferenceEventEntityUuid = newEvent ^. entityUuid
      , _editURLReferenceEventUrl = applyValue oldEvent newEvent url
      , _editURLReferenceEventLabel = applyValue oldEvent newEvent label
      , _editURLReferenceEventAnnotations = applyValue oldEvent newEvent annotations
      , _editURLReferenceEventCreatedAt = newEvent ^. createdAt
      }
  simpleSquashEvent (EditCrossReferenceEvent' oldEvent) (EditCrossReferenceEvent' newEvent) =
    EditCrossReferenceEvent' $
    EditCrossReferenceEvent
      { _editCrossReferenceEventUuid = newEvent ^. uuid
      , _editCrossReferenceEventParentUuid = newEvent ^. parentUuid
      , _editCrossReferenceEventEntityUuid = newEvent ^. entityUuid
      , _editCrossReferenceEventTargetUuid = applyValue oldEvent newEvent targetUuid
      , _editCrossReferenceEventDescription = applyValue oldEvent newEvent description
      , _editCrossReferenceEventAnnotations = applyValue oldEvent newEvent annotations
      , _editCrossReferenceEventCreatedAt = newEvent ^. createdAt
      }
