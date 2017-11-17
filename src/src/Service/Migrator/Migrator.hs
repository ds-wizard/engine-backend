module Service.Migrator.Migrator where

import Control.Lens
import Data.Maybe

import Common.Error
import Model.Event.Common
import Model.Event.Event
import Model.Event.KnowledgeModel.EditKnowledgeModelEvent
import Model.KnowledgeModel.KnowledgeModel
import Service.Migrator.Applicator

ncmApplyEvent
  :: Either AppError (Maybe KnowledgeModel)
  -> Event
  -> Either AppError (Maybe KnowledgeModel)
ncmApplyEvent emKM (AddKnowledgeModelEvent' e) = applyEventToKM e emKM
ncmApplyEvent emKM (EditKnowledgeModelEvent' e) = applyEventToKM e emKM
ncmApplyEvent emKM (AddChapterEvent' e) = applyEventToKM e emKM
ncmApplyEvent emKM (EditChapterEvent' e) = applyEventToKM e emKM
ncmApplyEvent emKM (DeleteChapterEvent' e) = applyEventToKM e emKM
ncmApplyEvent emKM (AddQuestionEvent' e) = applyEventToKM e emKM
ncmApplyEvent emKM (EditQuestionEvent' e) = applyEventToKM e emKM
ncmApplyEvent emKM (DeleteQuestionEvent' e) = applyEventToKM e emKM
ncmApplyEvent emKM (AddAnswerEvent' e) = applyEventToKM e emKM
ncmApplyEvent emKM (EditAnswerEvent' e) = applyEventToKM e emKM
ncmApplyEvent emKM (DeleteAnswerEvent' e) = applyEventToKM e emKM
ncmApplyEvent emKM (AddExpertEvent' e) = applyEventToKM e emKM
ncmApplyEvent emKM (EditExpertEvent' e) = applyEventToKM e emKM
ncmApplyEvent emKM (DeleteExpertEvent' e) = applyEventToKM e emKM
ncmApplyEvent emKM (AddReferenceEvent' e) = applyEventToKM e emKM
ncmApplyEvent emKM (EditReferenceEvent' e) = applyEventToKM e emKM
ncmApplyEvent emKM (DeleteReferenceEvent' e) = applyEventToKM e emKM
ncmApplyEvent emKM (AddFollowUpQuestionEvent' e) = applyEventToKM e emKM
ncmApplyEvent emKM (EditFollowUpQuestionEvent' e) = applyEventToKM e emKM
ncmApplyEvent emKM (DeleteFollowUpQuestionEvent' e) = applyEventToKM e emKM

noConflictMethod
  :: Either AppError (Maybe KnowledgeModel)
  -> Event
  -> Either AppError (Maybe KnowledgeModel)
noConflictMethod (Right mKm) event = ncmApplyEvent (Right mKm) event
noConflictMethod (Left error) _ = Left error

migrate :: Maybe KnowledgeModel -> [Event] -> Either AppError KnowledgeModel
migrate mKM events =
  case foldl noConflictMethod (Right mKM) events of
    Left error -> Left error
    Right Nothing ->
      Left . MigratorError $
      "Unspecified problem in building Knowledge Model happened"
    Right (Just km) -> Right km
