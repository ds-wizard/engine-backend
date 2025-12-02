module Wizard.Service.KnowledgeModel.Squash.Event.Expert where

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Expert.ExpertEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditExpertEvent where
  isSimpleEventSquashApplicable _ = True
  isReorderEventSquashApplicable _ _ = False
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) =
    createSquashedEvent oldEvent newEvent $
      EditExpertEvent'
        EditExpertEvent
          { name = applyValue oldContent newContent (.name)
          , email = applyValue oldContent newContent (.email)
          , annotations = applyValue oldContent newContent (.annotations)
          }
