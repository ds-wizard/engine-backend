module Wizard.Service.KnowledgeModel.Squash.Event.Reference where

import Shared.Model.Event.Reference.ReferenceEvent
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditReferenceEvent where
  isSimpleEventSquashApplicable _ = True
  isReorderEventSquashApplicable _ _ = False

  --  --------------------------------------
  isTypeChanged (EditResourcePageReferenceEvent' oldEvent) (EditResourcePageReferenceEvent' newEvent) = False
  isTypeChanged (EditURLReferenceEvent' oldEvent) (EditURLReferenceEvent' newEvent) = False
  isTypeChanged (EditCrossReferenceEvent' oldEvent) (EditCrossReferenceEvent' newEvent) = False
  isTypeChanged _ _ = True

  --  --------------------------------------
  simpleSquashEvent previousEvent (EditResourcePageReferenceEvent' oldEvent) (EditResourcePageReferenceEvent' newEvent) =
    EditResourcePageReferenceEvent' $
      EditResourcePageReferenceEvent
        { uuid = newEvent.uuid
        , parentUuid = newEvent.parentUuid
        , entityUuid = newEvent.entityUuid
        , shortUuid = applyValue oldEvent newEvent (.shortUuid)
        , annotations = applyValue oldEvent newEvent (.annotations)
        , createdAt = newEvent.createdAt
        }
  simpleSquashEvent previousEvent (EditURLReferenceEvent' oldEvent) (EditURLReferenceEvent' newEvent) =
    EditURLReferenceEvent' $
      EditURLReferenceEvent
        { uuid = newEvent.uuid
        , parentUuid = newEvent.parentUuid
        , entityUuid = newEvent.entityUuid
        , url = applyValue oldEvent newEvent (.url)
        , aLabel = applyValue oldEvent newEvent (.aLabel)
        , annotations = applyValue oldEvent newEvent (.annotations)
        , createdAt = newEvent.createdAt
        }
  simpleSquashEvent previousEvent (EditCrossReferenceEvent' oldEvent) (EditCrossReferenceEvent' newEvent) =
    EditCrossReferenceEvent' $
      EditCrossReferenceEvent
        { uuid = newEvent.uuid
        , parentUuid = newEvent.parentUuid
        , entityUuid = newEvent.entityUuid
        , targetUuid = applyValue oldEvent newEvent (.targetUuid)
        , description = applyValue oldEvent newEvent (.description)
        , annotations = applyValue oldEvent newEvent (.annotations)
        , createdAt = newEvent.createdAt
        }
