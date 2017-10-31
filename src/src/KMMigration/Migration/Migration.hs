module KMMigration.Migration.Migration where

import Control.Lens

import KMMigration.Migration.Applicator.Applicator
import KMMigration.Migration.Event.Common
import Model.Event.Event
import Model.Event.KnowledgeModel.EditKnowledgeModelEvent
import KMMigration.Model.Common
import Model.KnowledgeModel.KnowledgeModel

ncmApplyEvent :: KnowledgeModel -> Event -> KnowledgeModel
ncmApplyEvent km (MkEvent e) = applyEventToKM e km

noConflictMethod :: KnowledgeModel -> Event -> KnowledgeModel
noConflictMethod = ncmApplyEvent

migrate :: KnowledgeModel -> [Event] -> KnowledgeModel
migrate = foldl noConflictMethod
