module Service.KnowledgeModel.Compilator.Compilator
  ( compile
  ) where

import Model.Error.Error
import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelDM
import Service.KnowledgeModel.Compilator.EventApplicator.Answer ()
import Service.KnowledgeModel.Compilator.EventApplicator.Chapter ()
import Service.KnowledgeModel.Compilator.EventApplicator.EventApplicator
import Service.KnowledgeModel.Compilator.EventApplicator.Expert ()
import Service.KnowledgeModel.Compilator.EventApplicator.Integration
       ()
import Service.KnowledgeModel.Compilator.EventApplicator.KnowledgeModel
       ()
import Service.KnowledgeModel.Compilator.EventApplicator.Question
       ()
import Service.KnowledgeModel.Compilator.EventApplicator.Reference
       ()
import Service.KnowledgeModel.Compilator.EventApplicator.Tag ()

compile :: Maybe KnowledgeModel -> [Event] -> Either AppError KnowledgeModel
compile (Just km) events = foldl foldEvent (Right km) events
compile Nothing events = foldl foldEvent (Right defaultKnowledgeModel) events

-- --------------------------------
-- PRIVATE
-- --------------------------------
foldEvent :: Either AppError KnowledgeModel -> Event -> Either AppError KnowledgeModel
foldEvent (Right km) (AddKnowledgeModelEvent' e) = apply e km
foldEvent (Right km) (EditKnowledgeModelEvent' e) = apply e km
foldEvent (Right km) (AddChapterEvent' e) = apply e km
foldEvent (Right km) (EditChapterEvent' e) = apply e km
foldEvent (Right km) (DeleteChapterEvent' e) = apply e km
foldEvent (Right km) (AddQuestionEvent' e) = apply e km
foldEvent (Right km) (EditQuestionEvent' e) = apply e km
foldEvent (Right km) (DeleteQuestionEvent' e) = apply e km
foldEvent (Right km) (AddAnswerEvent' e) = apply e km
foldEvent (Right km) (EditAnswerEvent' e) = apply e km
foldEvent (Right km) (DeleteAnswerEvent' e) = apply e km
foldEvent (Right km) (AddExpertEvent' e) = apply e km
foldEvent (Right km) (EditExpertEvent' e) = apply e km
foldEvent (Right km) (DeleteExpertEvent' e) = apply e km
foldEvent (Right km) (AddReferenceEvent' e) = apply e km
foldEvent (Right km) (EditReferenceEvent' e) = apply e km
foldEvent (Right km) (DeleteReferenceEvent' e) = apply e km
foldEvent (Right km) (AddTagEvent' e) = apply e km
foldEvent (Right km) (EditTagEvent' e) = apply e km
foldEvent (Right km) (DeleteTagEvent' e) = apply e km
foldEvent (Right km) (AddIntegrationEvent' e) = apply e km
foldEvent (Right km) (EditIntegrationEvent' e) = apply e km
foldEvent (Right km) (DeleteIntegrationEvent' e) = apply e km
foldEvent (Left error) _ = Left error
