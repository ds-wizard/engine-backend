module Wizard.Service.KnowledgeModel.Squash.Event.Choice where

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Choice.ChoiceEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditChoiceEvent where
  isSimpleEventSquashApplicable _ = True
  isReorderEventSquashApplicable _ _ = False
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) =
    createSquashedEvent oldEvent newEvent $
      EditChoiceEvent'
        EditChoiceEvent
          { aLabel = applyValue oldContent newContent (.aLabel)
          , annotations = applyValue oldContent newContent (.annotations)
          }
