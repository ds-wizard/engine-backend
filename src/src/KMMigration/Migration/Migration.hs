module KMMigration.Migration.Migration where

import Control.Lens
import Data.Maybe

import Common.Error
import KMMigration.Migration.Applicator.Applicator
import Model.Event.Common
import Model.Event.Event
import Model.Event.KnowledgeModel.EditKnowledgeModelEvent
import Model.KnowledgeModel.KnowledgeModel

ncmApplyEvent :: Maybe KnowledgeModel -> Event -> Maybe KnowledgeModel
ncmApplyEvent mKM (AddKnowledgeModelEvent' e) = Just $ applyEventToKM e mKM
ncmApplyEvent mKM (EditKnowledgeModelEvent' e) = Just $ applyEventToKM e mKM
ncmApplyEvent mKM (AddChapterEvent' e) = Just $ applyEventToKM e mKM
ncmApplyEvent mKM (EditChapterEvent' e) = Just $ applyEventToKM e mKM
ncmApplyEvent mKM (DeleteChapterEvent' e) = Just $ applyEventToKM e mKM
ncmApplyEvent mKM (AddQuestionEvent' e) = Just $ applyEventToKM e mKM
ncmApplyEvent mKM (EditQuestionEvent' e) = Just $ applyEventToKM e mKM
ncmApplyEvent mKM (DeleteQuestionEvent' e) = Just $ applyEventToKM e mKM
ncmApplyEvent mKM (AddAnswerEvent' e) = Just $ applyEventToKM e mKM
ncmApplyEvent mKM (EditAnswerEvent' e) = Just $ applyEventToKM e mKM
ncmApplyEvent mKM (DeleteAnswerEvent' e) = Just $ applyEventToKM e mKM
ncmApplyEvent mKM (AddExpertEvent' e) = Just $ applyEventToKM e mKM
ncmApplyEvent mKM (EditExpertEvent' e) = Just $ applyEventToKM e mKM
ncmApplyEvent mKM (DeleteExpertEvent' e) = Just $ applyEventToKM e mKM
ncmApplyEvent mKM (AddReferenceEvent' e) = Just $ applyEventToKM e mKM
ncmApplyEvent mKM (EditReferenceEvent' e) = Just $ applyEventToKM e mKM
ncmApplyEvent mKM (DeleteReferenceEvent' e) = Just $ applyEventToKM e mKM
ncmApplyEvent mKM (AddFollowUpQuestionEvent' e) = Just $ applyEventToKM e mKM
ncmApplyEvent mKM (EditFollowUpQuestionEvent' e) = Just $ applyEventToKM e mKM
ncmApplyEvent mKM (DeleteFollowUpQuestionEvent' e) = Just $ applyEventToKM e mKM

noConflictMethod :: Maybe KnowledgeModel -> Event -> Maybe KnowledgeModel
noConflictMethod = ncmApplyEvent

migrate :: Maybe KnowledgeModel -> [Event] -> Either AppError KnowledgeModel
migrate mKM events = Right $ fromJust $ foldl noConflictMethod mKM events
