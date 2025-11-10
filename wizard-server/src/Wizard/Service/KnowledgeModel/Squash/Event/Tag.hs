module Wizard.Service.KnowledgeModel.Squash.Event.Tag where

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Tag.TagEvent
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditTagEvent where
  isSimpleEventSquashApplicable _ = True
  isReorderEventSquashApplicable _ _ = False
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) =
    createSquashedEvent oldEvent newEvent $
      EditTagEvent'
        EditTagEvent
          { name = applyValue oldContent newContent (.name)
          , description = applyValue oldContent newContent (.description)
          , color = applyValue oldContent newContent (.color)
          , annotations = applyValue oldContent newContent (.annotations)
          }
