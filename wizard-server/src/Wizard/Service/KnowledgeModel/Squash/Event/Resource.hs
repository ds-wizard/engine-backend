module Wizard.Service.KnowledgeModel.Squash.Event.Resource where

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Resource.ResourceEvent
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditResourceCollectionEvent where
  isSimpleEventSquashApplicable = not . isChanged resourcePageUuids
  isReorderEventSquashApplicable (previousEvent, _) (newEvent, _) = previousEvent.entityUuid == newEvent.entityUuid
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) =
    createSquashedEvent oldEvent newEvent $
      EditResourceCollectionEvent'
        EditResourceCollectionEvent
          { title = applyValue oldContent newContent (.title)
          , resourcePageUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.resourcePageUuids)
          , annotations = applyValue oldContent newContent (.annotations)
          }

instance SimpleEventSquash EditResourcePageEvent where
  isSimpleEventSquashApplicable _ = True
  isReorderEventSquashApplicable _ _ = False
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) =
    createSquashedEvent oldEvent newEvent $
      EditResourcePageEvent'
        EditResourcePageEvent
          { title = applyValue oldContent newContent (.title)
          , content = applyValue oldContent newContent (.content)
          , annotations = applyValue oldContent newContent (.annotations)
          }
