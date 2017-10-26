module KMMigration.Migration.Migration where

import Control.Lens

import KMMigration.Migration.Applicator.Applicator
import KMMigration.Migration.Event.Common
import KMMigration.Migration.Event.EventApplicable
import KMMigration.Migration.Event.KnowledgeModel.EditKnowledgeModelEvent
import KMMigration.Model.Common
import KMMigration.Model.Event
import KMMigration.Model.KnowledgeModel

ncmApplyEvent :: KnowledgeModel -> EventApplicable -> KnowledgeModel
ncmApplyEvent km (MkEventApplicable e) = applyEventToKM e km

noConflictMethod :: KnowledgeModel -> EventApplicable -> KnowledgeModel
noConflictMethod = ncmApplyEvent

migrate :: KnowledgeModel -> [EventApplicable] -> KnowledgeModel
migrate = foldl noConflictMethod
