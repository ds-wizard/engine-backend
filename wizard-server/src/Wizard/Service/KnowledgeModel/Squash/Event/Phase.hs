module Wizard.Service.KnowledgeModel.Squash.Event.Phase where

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Phase.PhaseEvent
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditPhaseEvent where
  isSimpleEventSquashApplicable _ = True
  isReorderEventSquashApplicable _ _ = False
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) =
    createSquashedEvent oldEvent newEvent $
      EditPhaseEvent'
        EditPhaseEvent
          { title = applyValue oldContent newContent (.title)
          , description = applyValue oldContent newContent (.description)
          , annotations = applyValue oldContent newContent (.annotations)
          }
