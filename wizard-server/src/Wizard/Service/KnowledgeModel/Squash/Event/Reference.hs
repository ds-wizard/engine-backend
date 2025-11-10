module Wizard.Service.KnowledgeModel.Squash.Event.Reference where

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Reference.ReferenceEvent
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
  simpleSquashEvent previousEvent (oldEvent, EditResourcePageReferenceEvent' oldContent) (newEvent, EditResourcePageReferenceEvent' newContent) =
    createSquashedEvent oldEvent newEvent $
      EditReferenceEvent' $
        EditResourcePageReferenceEvent' $
          EditResourcePageReferenceEvent
            { resourcePageUuid = applyValue oldContent newContent (.resourcePageUuid)
            , annotations = applyValue oldContent newContent (.annotations)
            }
  simpleSquashEvent previousEvent (oldEvent, EditURLReferenceEvent' oldContent) (newEvent, EditURLReferenceEvent' newContent) =
    createSquashedEvent oldEvent newEvent $
      EditReferenceEvent' $
        EditURLReferenceEvent' $
          EditURLReferenceEvent
            { url = applyValue oldContent newContent (.url)
            , aLabel = applyValue oldContent newContent (.aLabel)
            , annotations = applyValue oldContent newContent (.annotations)
            }
  simpleSquashEvent previousEvent (oldEvent, EditCrossReferenceEvent' oldContent) (newEvent, EditCrossReferenceEvent' newContent) =
    createSquashedEvent oldEvent newEvent $
      EditReferenceEvent' $
        EditCrossReferenceEvent' $
          EditCrossReferenceEvent
            { targetUuid = applyValue oldContent newContent (.targetUuid)
            , description = applyValue oldContent newContent (.description)
            , annotations = applyValue oldContent newContent (.annotations)
            }
  simpleSquashEvent previousEvent oldEvent newEvent = error $ "Simple squash event is not applicable for " <> show (oldEvent, newEvent) <> " in " <> show previousEvent
