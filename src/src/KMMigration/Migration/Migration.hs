module KMMigration.Migration.Migration where

import Control.Lens

import KMMigration.Migration.Applicator.Applicator
import Model.Event.Common
import Model.Event.Event
import Model.Event.KnowledgeModel.EditKnowledgeModelEvent
import Model.KnowledgeModel.KnowledgeModel

ncmApplyEvent :: KnowledgeModel -> Event -> KnowledgeModel
ncmApplyEvent km (AddKnowledgeModelEvent' e) = applyEventToKM e km
ncmApplyEvent km (EditKnowledgeModelEvent' e) = applyEventToKM e km
ncmApplyEvent km (AddChapterEvent' e) = applyEventToKM e km
ncmApplyEvent km (EditChapterEvent' e) = applyEventToKM e km
ncmApplyEvent km (DeleteChapterEvent' e) = applyEventToKM e km
ncmApplyEvent km (AddQuestionEvent' e) = applyEventToKM e km
ncmApplyEvent km (EditQuestionEvent' e) = applyEventToKM e km
ncmApplyEvent km (DeleteQuestionEvent' e) = applyEventToKM e km
ncmApplyEvent km (AddAnswerEvent' e) = applyEventToKM e km
ncmApplyEvent km (EditAnswerEvent' e) = applyEventToKM e km
ncmApplyEvent km (DeleteAnswerEvent' e) = applyEventToKM e km
ncmApplyEvent km (AddExpertEvent' e) = applyEventToKM e km
ncmApplyEvent km (EditExpertEvent' e) = applyEventToKM e km
ncmApplyEvent km (DeleteExpertEvent' e) = applyEventToKM e km
ncmApplyEvent km (AddReferenceEvent' e) = applyEventToKM e km
ncmApplyEvent km (EditReferenceEvent' e) = applyEventToKM e km
ncmApplyEvent km (DeleteReferenceEvent' e) = applyEventToKM e km
ncmApplyEvent km (AddFollowUpQuestionEvent' e) = applyEventToKM e km
ncmApplyEvent km (EditFollowUpQuestionEvent' e) = applyEventToKM e km
ncmApplyEvent km (DeleteFollowUpQuestionEvent' e) = applyEventToKM e km

noConflictMethod :: KnowledgeModel -> Event -> KnowledgeModel
noConflictMethod = ncmApplyEvent

migrate :: KnowledgeModel -> [Event] -> KnowledgeModel
migrate = foldl noConflictMethod
